open Ast
open Sast

exception TODO of string

module StringMap = Map.Make(String)

let check (tdecls, fdecls) =
  let check_binds (kind : string) (to_check : bind list) =
    let name_compare (_, n1) (_, n2) = compare n1 n2 in
    let check_it checked binding =
      let void_err = "illegal void " ^ kind ^ " " ^ snd binding
      and dup_err = "duplicate " ^ kind ^ " " ^ snd binding
      in match binding with
        (* No void bindings *)
        (Void, _) -> raise (Failure void_err)
      | (_, n1) -> match checked with
                    (* No duplicate bindings *)
                      ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
                    | _ -> binding :: checked

    in let _ = List.fold_left check_it [] (List.sort name_compare to_check)
       in to_check
  in

  let fmap =
    let add_bind map (name, formal_type, return_type) = StringMap.add name
      { fname = name;
        formals = [(formal_type, "x")];
        body = [];
        ret_type = return_type } map
    (*
     * @TODO - we currently have a to_string function which takes any type. But
     * our language is statically type
     *)
    (* TODO: - Add more builtin functions *)
    in List.fold_left add_bind StringMap.empty
       [("print", String, Void);
        ("string_of_int", Int, String);
        ("string_of_float", Float, String);
        ("string_of_bool", Bool, String)]
  in
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
      and dup_err = "duplicate function " ^ fd.fname
      and make_err er = raise (Failure er)
      and n = fd.fname
    in match fd with
      _ when StringMap.mem n map -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ -> StringMap.add n fd map
  in
  let function_decls = List.fold_left add_func fmap fdecls
  in

  let add_thread map thread_def =
    let dup_error = "duplicate thread " ^ thread_def.tname
      and tname = thread_def.tname
    in match thread_def with
        _ when StringMap.mem tname map -> raise (Failure dup_error)
      | _ -> StringMap.add tname thread_def map
  in
  let thread_defs = List.fold_left add_thread StringMap.empty tdecls in

  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let find_thread_def s =
    try StringMap.find s thread_defs
    with Not_found -> raise (Failure ("unrecognized thread definition " ^ s))
  in

  let _ = find_thread_def "Main" in

  let check_assign lvaluet rvaluet e =
    let err = "illegal argument found " ^ string_of_typ rvaluet ^ " expected " ^
              string_of_typ lvaluet ^ " in " ^ string_of_expr e
    in if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in let rec lookup id envs =
    match envs with
      [] -> raise (Failure ("variable " ^ id ^ " not found"))
      | env :: envs -> try StringMap.find id env with Not_found -> lookup id envs
  and bind id typ envs =
    match envs with
      [] -> raise (Failure ("Implementation bug: empty environments"))
      | env :: envs -> (StringMap.add id typ env) :: envs
  in let rec check_stmt (envs: 'a StringMap.t list) (stmt: stmt) =
    match stmt with
      Block stmts ->
        (*
         * When going over the list of statements, a new block potentially create a new scoping,
         * so we need to add a new environment. The new environment is discarded after the block.
         *)
        let rec check_stmt_list envs stmts = match stmts with
              [Return _ as return] -> let (_, sstmt) = check_stmt envs return in [sstmt]
            | Return _ :: _ -> raise (Failure("Nothing may following a return"))
            | Block _ as block :: stmts ->
                let (_, sstmt) = check_stmt (StringMap.empty :: envs) block and
                    sstmts = check_stmt_list envs stmts in (sstmt :: sstmts)
            | stmt :: stmts ->
                let (envs', sstmt) = check_stmt envs stmt in
                let sstmts = check_stmt_list envs' stmts in (sstmt :: sstmts)
            | [] -> []
        in (envs, SBlock (check_stmt_list envs stmts))
      | Expr expr -> let sexpr = check_expr envs expr in (envs, SExpr sexpr)
      | If (expr, stmt1, stmt2) ->
          let (typ, _) as sexpr = check_expr envs expr in
          let _ = check_assign Bool typ expr in
          let (_, sstmt1) = check_stmt envs stmt1 in
          let (_, sstmt2) = check_stmt envs stmt2 in
          (envs, SIf (sexpr, sstmt1, sstmt2))
      | For (pre_loop_stmt, pre_loop_expr, post_loop_expr, body) ->
          let (envs', pre_loop_sstmt) = check_stmt envs pre_loop_stmt in
          let (typ, _) as pre_loop_sexpr = check_expr envs' pre_loop_expr and
              post_loop_expr = check_expr envs' post_loop_expr and
              (_, body_sexpr) = check_stmt envs' body in
          let _ = check_assign Bool typ pre_loop_expr in
            (envs, SFor (pre_loop_sstmt, pre_loop_sexpr, post_loop_expr, body_sexpr))
      | While (expr, body) ->
          let (typ, _) as sexpr = check_expr envs expr and
              (_, sbody) = check_stmt envs body in
          let _ = check_assign Bool typ expr in (envs, SWhile (sexpr, sbody))
      | Send (id, expr) ->
          let typ = lookup id envs in
          let _ = check_assign Thread typ expr in
          let sexpr = check_expr envs expr in
          (envs, SSend (id, sexpr))
      | Decl (lt, id, expr) ->
        (match id with
          "parent" | "self" -> raise (Failure "parent/self is a keyword")
          | _ as id ->  let _ = check_binds "local" [(lt, id)] in
                  (match envs with
                    env :: _ ->
                      if StringMap.mem id env then raise (Failure (id ^ " exists in scope"))
                      else let (rt, e') as sexpr = check_expr envs expr in
                      (match e' with
                        | SNoexpr -> (bind id lt envs, SDecl (lt, id, sexpr))
                        | _ -> (bind id lt envs, SDecl (check_assign lt rt expr, id, (lt, e'))))
                    | [] -> raise (Failure "Implementation bug: empty environments")))
      | Break -> (envs, SBreak)
      | Continue -> (envs, SContinue)
      | Receive receive_cases ->
          (*
           * @TODO - Check that all patterns are unique
           *)
          (* Extend the environment with pattern *)
          let rec extend_env env = function
              WildcardPattern -> env
            | BasePattern (typ, id) ->
                let _ = check_binds "patterns" [(typ, id)] in
                if StringMap.mem id env then raise (Failure (id ^ " exists in scope"))
                else StringMap.add id typ env
            | TuplePattern (pattern1, pattern2) ->
                let env' = extend_env env pattern1 in extend_env env' pattern2
          (* Convert AST pattern to SAST pattern *)
          in let rec get_sast_pattern = function
              BasePattern (typ, id) -> SBasePattern (typ, id)
            | WildcardPattern -> SWildcardPattern
            | TuplePattern (p1, p2) -> STuplePattern (get_sast_pattern p1, get_sast_pattern p2)
          (* For each case block, extends the environment and perform semantic check *)
          in let check_receive_case (pattern, stmt) =
            let env' = extend_env StringMap.empty pattern in
            let (_, sstmt) = check_stmt (env' :: envs) stmt in (get_sast_pattern pattern, sstmt)

          (* Check if a wilcard pattern exists *)
          in if (List.exists (fun (p, _) -> p = WildcardPattern) receive_cases) then
            (* Semantically check each pattern *)
            (envs, SReceive (List.map check_receive_case receive_cases))
          else raise (Failure "Pattern does not contain wildcard")
      | Return expr -> (envs, SReturn (check_expr envs expr))
  and check_expr (envs: 'a StringMap.t list) (expr: expr) =
    match expr with
      IntLit n -> (Int, SIntLit n)
      | StringLit s -> (String, SStringLit s)
      | FloatLit n -> (Float, SFloatLit n)
      | BoolLit b -> (Bool, SBoolLit b)
      | TupleLit (e1, e2) ->
          let (t1, sexpr1) = check_expr envs e1 and
              (t2, sexpr2) = check_expr envs e2
          in (Tuple (t1, t2), STupleLit ((t1, sexpr1), (t2, sexpr2)))
      (*
       * @TODO - How do we deal with an empty array declaration?
       *)
      | ArrayLit _ -> raise (TODO "Implement array literal")
          (* let sexprs = List.map (check_expr envs) xs in
          (* let ty_check (t1, _) =  *)
          let ty_check sexprs = match sexprs with
            | [] -> true
            | ((t1, _) :: rest) -> List.iter check_assign  *)

          (* (match (List.map (fun x -> check_expr envs x) xs) with
            [] -> (Array (Void, 0), SArrayLit [])
            | (sexpr :: sexprs) as list_sexprs ->
                let (lt, _) = sexpr in
                let _ = List.iter (fun (rt, _) -> check_assign )
                if List.for_all (fun (rt, _) -> check_assign ) sexprs then
                (Array (lt, List.length list_sexprs), SArrayLit list_sexprs)
                else raise (Failure ()) *)
      | Call (fname, args) as call ->
        let fd = find_func fname in
        let _ = check_binds "formals" fd.formals in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call sargs (ft, _) e =
          let (et, e') = check_expr envs e in (check_assign ft et e, e') :: sargs
        in let sargs = List.fold_left2 check_call [] fd.formals args
        in (fd.ret_type, SCall (fname, List.rev sargs))
      | Binop(e1, op, e2) as e ->
          let (t1, e1') = check_expr envs e1 in
          let (t2, e2') = check_expr envs e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div | Mod | Pow when same && t1 = Int -> Int
          | Add | Sub | Mult | Div | Pow       when same && t1 = Float -> Float
          | Equality | Neq                     when same               -> Bool
          | Less | Leq | Greater | Geq
                                               when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or                           when same && t1 = Bool -> Bool
          | Add                                when same && t1 = String -> String
          | _ -> raise (Failure ("illegal binary operator " ^
                          string_of_typ t1 ^ string_of_op op ^
                          string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Id s -> let t = (lookup s envs) in (t, SId s)
      | Spawn t -> let _ = find_thread_def t in (Thread, SSpawn t)
      | Assign (id, expr) ->
          let rt = lookup id envs
          and (lt, sexpr) = check_expr envs expr
          in (check_assign lt rt expr, SAssign (id, (lt, sexpr)))
      | Noexpr -> (Void, SNoexpr)
      | Unit -> raise (Failure "semantic unit")
      | PrefixUnop (op, expr) ->
          let (t, _) as e' = check_expr envs expr in
          (match op with
              Neg ->
                let operand = match t with
                    Int -> IntLit (-1)
                  | Float -> FloatLit "-1.0"
                  | typ -> raise (Failure (string_of_typ typ ^ " can't be negated")) in
                check_expr envs (Binop (expr, Mult, operand))
            | Not -> ((check_assign Bool t expr), SPreUnop (op, e'))
            | _ -> raise (Failure "Parsing bug"))
      | PostfixUnop (op, expr) ->
          let (t, e') = check_expr envs expr in
          let id = match e' with
              SId id -> id
            | _ -> raise (Failure "Expected ID in postfix operation") in
          let op = match op with
              Plusplus -> Add
            | Minmin -> Sub
            | _ -> raise (Failure "Parsing bug")
          in (match t with
            Int -> check_expr envs (Assign (id, Binop (expr, op, IntLit 1)))
            | Float -> check_expr envs (Assign (id, Binop (expr, op, FloatLit "1.0")))
            | Semaphore -> raise (Failure "semaphore")
            | typ -> raise (Failure (string_of_typ typ ^ " can't be assigned to postfix operation")))
      | AssignIndex (_, _, _) -> raise (Failure "semantic assignIndex")
      | Index (_, _) -> raise (Failure "semantic index")

  (*
   * Check that break and continue statements are inside for and while loop
   *)
  in let rec loop_check = function
    | SBlock sl -> List.iter loop_check sl
    | SFor _ -> ()
    | SWhile _ -> ()
    | SBreak -> raise (Failure ("break statement must be inside a for/while loop"))
    | SContinue -> raise (Failure ("continue statement must be inside a for/while loop"))
    | _ -> ()
  (*
   * Check that thread do not have return statements and function return type is equivalent
   * to function return type
   *)
  in let return_check is_thread name =
    let rec checker = function
      | SBlock sl -> List.iter checker sl
      | SReturn (typ, _) ->
          if is_thread then raise (Failure ("return statement found in thread " ^ name)) else
          let fdecl = StringMap.find name function_decls in
          if typ = fdecl.ret_type then () else
          raise (Failure ("return has " ^ string_of_typ typ ^ " but expected " ^ string_of_typ fdecl.ret_type))
      | _ -> ()
    in checker

  in let check_function (fdecl: func_decl) =
    let formals = check_binds "formal" fdecl.formals in
    let env = List.fold_left (fun map (typ, id) -> StringMap.add id typ map) StringMap.empty formals in
    let (_, sstmt) = check_stmt [env] (Block fdecl.body)
    in match sstmt with
      SBlock (sl) ->
        (*
         * @TODO - Talk about thread spawning for functions because it requires
         * functions to implicitly accepts 2 message pools
         *)
        let _ = List.iter loop_check sl in
        let _ = List.iter (fun sstmt -> return_check false fdecl.fname sstmt) sl in
        { sfname = fdecl.fname; sformals = fdecl.formals; sbody = sl; sret_type = fdecl.ret_type }
      | _ -> raise (Failure "Failed to parsed function")


  in let check_thread (tdecl: thread_decl) =
    let env = StringMap.empty in
    let env = StringMap.add "parent" Thread env in
    let env = StringMap.add "self" Thread env in
    let (_, sstmt) = check_stmt [env] (Block tdecl.body)
    in match sstmt with
      SBlock (sl) ->
        let _ = List.iter loop_check sl in
        let _ = List.iter (fun sstmt -> return_check true tdecl.tname sstmt) sl in
        { stname = tdecl.tname; sbody = sl }
      | _ -> raise (Failure "Failed to parsed thread")

  in (List.map check_thread tdecls, List.map check_function fdecls)
