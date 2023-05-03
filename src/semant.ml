open Ast
open Sast

exception TODO of string

module StringMap = Map.Make(String)

let check (tdecls, fdecls) =
  let rec check_lit expr = function
      Void -> raise (Failure ("illegal void in " ^ string_of_expr expr))
    | Array (typ, _) -> check_lit expr typ
    | Tuple (t1, t2) ->
        let _ = check_lit expr t1 and
            _ = check_lit expr t2 in ()
    | _ -> () in
  let check_binds (kind : string) (to_check : bind list) =
    let name_compare (_, n1) (_, n2) = compare n1 n2 in
    let check_name checked binding = match binding with
      | (_, n1) ->
          match checked with
              ((_, n2) :: _) when n1 = n2 -> raise (Failure ("duplicate " ^ kind ^ " " ^ snd binding))
            | _ -> binding :: checked  in
    let rec check_typ id = function
        Void -> raise (Failure ("illegal void " ^ kind ^ " " ^ id))
      | Array (typ, _) -> check_typ id typ
      | Tuple (t1, t2) ->
          let _ = check_typ id t1 and
              _ = check_typ id t2 in ()
      | _ -> () in
    let _ = List.fold_left check_name [] (List.sort name_compare to_check) in
    let _ = List.iter (fun (typ, id) -> check_typ id typ) to_check in to_check
  in let fmap =
    let add_bind map (name, formals, return_type) = StringMap.add name
      { fname = name;
        formals = formals;
        body = [];
        ret_type = return_type } map
    (* TODO: - Add more builtin functions *)
    in List.fold_left add_bind StringMap.empty
       [("print", [(String, "x")], Void);
        ("string_of_int", [(Int, "x")], String);
        ("string_of_float", [(Float, "x")], String);
        ("string_of_bool", [(Bool, "x")], String);
        ("make_semaphore", [(Int, "x")], Semaphore);
        ("exit", [(Int, "x")], Void);
        ("end", [], Void)];
  in
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
      and dup_err = "duplicate function " ^ fd.fname
      and make_err er = raise (Failure er)
      and n = fd.fname
      and _ = check_binds "fdecl" fd.formals
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
      | Decl (BaseDecl (lt, id, expr)) ->
          (match envs with
            env :: _ ->
              if StringMap.mem id env then raise (Failure (id ^ " exists in scope"))
              else let (rt, e') as sexpr = check_expr envs expr in
              let _ = check_binds "decl" [(lt, id)] in
              (match e' with
                | SNoexpr -> (bind id lt envs, SDecl (SBaseDecl (lt, id, sexpr)))
                | _ -> (bind id lt envs, SDecl (SBaseDecl (check_assign lt rt expr, id, (lt, e')))))
            | [] -> raise (Failure "Implementation bug: empty environments"))
      | Decl (TupleDecl (leftTup, rightTup, expr) as tupDecl) ->
          let (res_type, _) as sexpr = check_expr envs expr in
          let rec check_tuple_assign declType ty = match declType with
            | BaseDecl (t, _, _) -> if t != ty then raise (Failure ("Tuple type mismatch, expected a " ^ string_of_typ t ^ " but got a " ^ string_of_typ ty))
            | TupleDecl (leftTup, rightTup, _) ->
                (match ty with
                  | Tuple (t1, t2) ->
                    let _ = check_tuple_assign leftTup t1 in
                    let _ = check_tuple_assign rightTup t2 in ()
                  | _ -> raise (Failure ("Tuple type mismatch, expected a tuple but got a " ^ string_of_typ ty ^ " instead")))
          in
          let rec build_stuple = function
            | BaseDecl (t, id, _) ->
                let _ = check_binds "tuple_unpack" [(t, id)] in SBaseDecl (t, id, (t, SNoexpr))
            | TupleDecl (leftTup, rightTup, _) ->
                STupleDecl (build_stuple leftTup, build_stuple rightTup, (Void, SNoexpr))
          in let rec add_unpacked_vars envs decl =
            (match decl with
              BaseDecl _ as decl ->
                let (envs', _) = check_stmt envs (Decl decl) in
                envs'
            | TupleDecl (leftTup, rightTup, _) ->
                let envs' = add_unpacked_vars envs leftTup in add_unpacked_vars envs' rightTup)
          in
          let _ = check_tuple_assign tupDecl res_type in
          (add_unpacked_vars envs tupDecl, SDecl (STupleDecl (build_stuple leftTup, build_stuple rightTup, sexpr)))
      | Break -> (envs, SBreak)
      | Continue -> (envs, SContinue)
      | Receive receive_cases ->
          (* Check all patterns are unique by comparing each pattern with the rest *)
          let check_unique p =
            let dup_err p = "Pattern " ^ string_of_pattern p ^ " is not unique" in
            let patterns = List.map (fun (p, _) -> p) receive_cases in
            let rec eq_pattern lpattern rpattern = match (lpattern, rpattern) with
                (BasePattern (lt, _), BasePattern (rt, _)) -> lt = rt
              | (TuplePattern (ltuple1, ltuple2), TuplePattern (rtuple1, rtuple2)) ->
                  (eq_pattern ltuple1 rtuple1) && (eq_pattern ltuple2 rtuple2)
              | (WildcardPattern, WildcardPattern) -> true
              | _ -> false in
            let (to_check, remaining) = List.partition (fun other -> other = p) patterns in
            let to_check = match to_check with
                  [p] -> p
                | [] -> raise (Failure "Implement bug: Fail to extract pattern")
                | _ -> raise (Failure (dup_err p)) in

            if List.exists (fun pattern -> eq_pattern to_check pattern) remaining then raise (Failure (dup_err p))
            else () in
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

          in let _ = List.iter (fun (pattern, _) -> check_unique pattern) receive_cases
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
      | TupleLit (e1, e2) as tuple ->
          let (t1, sexpr1) = check_expr envs e1 and
              (t2, sexpr2) = check_expr envs e2 in
          (* Check that there is no void assignment *)
          let _ = List.iter (fun (typ, _) -> check_lit tuple typ) [t1, t2]
          in (Tuple (t1, t2), STupleLit ((t1, sexpr1), (t2, sexpr2)))
      | ArrayLit xs as array ->
          (* Semantically check each element *)
          let sexprs = List.map (check_expr envs) xs in
          (* Check that there is no void assignment *)
          let _ = List.iter (fun (typ, _) -> check_lit array typ) sexprs in
          (* Enforce that each element has the same type *)
          let typ = match sexprs with
              [] -> Void
            | ((lt, _) :: _) ->
                let _ = List.iter2 (fun (typ, _) e -> ignore (check_assign lt typ e)) sexprs xs in lt
          in (Array (typ, List.length xs), SArrayLit sexprs)
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
          let (t1, _) as lsexpr = check_expr envs e1 in
          let (t2, _) as rsexpr = check_expr envs e2 in

          if t1 = Semaphore then
            raise (Failure ("Expected postfix operators for semaphore " ^ string_of_expr e1 ^ " but found " ^ string_of_expr e2))
          else
            (* All binary operators, except semaphore, require operands of the same type *)
            let same = t1 = t2 in
            (* Determine expression type based on operator and operand types *)
            let ty = match op with
              Add | Sub | Mult | Div | Mod | Pow when same && t1 = Int -> Int
            | Add | Sub | Mult | Div             when same && t1 = Float -> Float
            | Pow                                when t1 = Float && t2 = Int -> Float
            | Equality | Neq                     when same               -> Bool
            | Less | Leq | Greater | Geq         when same && (t1 = Int || t1 = Float) -> Bool
            | And | Or                           when same && t1 = Bool -> Bool
            | Add                                when same && t1 = String -> String
            | _ -> raise (Failure ("illegal binary operator " ^
                            string_of_typ t1 ^ string_of_op op ^
                            string_of_typ t2 ^ " in " ^ string_of_expr e))
            in (ty, SBinop(lsexpr, op, rsexpr))
      | Id s -> let t = (lookup s envs) in (t, SId s)
      | Spawn t -> let _ = find_thread_def t in (Thread, SSpawn t)
      | Assign (id, expr) ->
          let lt = lookup id envs
          and (rt, sexpr) = check_expr envs expr
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
      | PostfixUnop (unop, expr) ->
          let (t, e') as sexpr = check_expr envs expr in
          let id = match e' with
              SId id -> id
            | _ -> raise (Failure "Expected ID in postfix operation") in
          let binop = match unop with
              Plusplus -> Add
            | Minmin -> Sub
            | _ -> raise (Failure "Parsing bug")
          in (match t with
            Int -> check_expr envs (Assign (id, Binop (expr, binop, IntLit 1)))
            | Float -> check_expr envs (Assign (id, Binop (expr, binop, FloatLit "1.0")))
            | Semaphore -> (t, SPostUnop (unop, sexpr))
            | typ -> raise (Failure (string_of_typ typ ^ " can't be assigned to postfix operation")))
      | AssignIndex (arr_expr, index, expr) ->
          let (typ, _) as arr_sexpr = check_expr envs arr_expr and
              (index_typ, _) as index_sexpr = check_expr envs index and
              (expr_typ, _) as assign_sexpr = check_expr envs expr in
          (match typ with
              Array (array_typ, _) ->
                let _ = check_assign Int index_typ index in
                let _ = check_assign array_typ expr_typ expr in
                (expr_typ, SAssignIndex (arr_sexpr, index_sexpr, assign_sexpr))
            | _ -> raise (Failure ("Expected an array for assign index but found " ^ string_of_typ typ)))
      | Index (expr, index) ->
          let (typ, _) as sexpr = check_expr envs expr in
          let (index_typ, _) as sindex = check_expr envs index in
          (match typ with
              Array (array_typ, _) ->
                let _ = check_assign Int index_typ index in
                (array_typ, SIndex (sexpr, sindex))
            | _ -> raise (Failure ("Expected an array but found " ^ string_of_typ typ)))
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
   * to function return type.
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
  (*
   * Check that a function has a return statement
   *)
  in let rec return_exist = function
    | SBlock sl -> List.exists return_exist sl
    | SReturn _ -> true
    | _ -> false
  (*
   * Check that parent / self is not redeclared
   *)
  in let rec keyword_check = function
    | SBlock sl -> List.iter keyword_check sl
    | SDecl (SBaseDecl (_, id, _)) ->
        (match id with
          "parent" | "self" -> raise (Failure "parent/self can't be declared in thread")
          | _ -> ())
    | SDecl (STupleDecl (ldecl, rdecl, _)) ->
        let _ = keyword_check (SDecl ldecl) in keyword_check (SDecl rdecl)
    | _ -> ()

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
        if return_exist sstmt then
          { sfname = fdecl.fname; sformals = fdecl.formals; sbody = sl; sret_type = fdecl.ret_type }
        else raise (Failure ("Function " ^ fdecl.fname ^ " does not have a return statement"))
      | _ -> raise (Failure "Failed to parsed function")


  in let check_thread (tdecl: thread_decl) =
    let env = StringMap.empty in
    let env = StringMap.add "parent" Thread env in
    let env = StringMap.add "self" Thread env in
    let (_, sstmt) = check_stmt [env] (Block tdecl.body)
    in match sstmt with
      SBlock (sl) ->
        let _ = List.iter loop_check sl in
        let _ = List.iter keyword_check sl in
        let _ = List.iter (fun sstmt -> return_check true tdecl.tname sstmt) sl in
        { stname = tdecl.tname; sbody = sl }
      | _ -> raise (Failure "Failed to parsed thread")

  in (List.map check_thread tdecls, List.map check_function fdecls)
