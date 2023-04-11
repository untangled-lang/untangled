open Ast
open Sast
(*
 * TODO:
 * Checking for a main thread
 * Adding a built-in functions
 * Checking for function calls (type and parameters)
 *)

 (*
  thread_def Main {
    print("Hello World!");
  }
 *)

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
        ("string_of_float", Float, String)]
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

(*
type sstmt =
SBlock of sstmt list
| SExpr of sexpr
| SReturn of sexpr
| SBreak
| SContinue
| SIf of sexpr * sstmt * sstmt
| SFor of sstmt * sexpr * sexpr * sstmt
| SWhile of sexpr * sstmt
| SSend of string * sexpr
| SSendParent of sexpr
| SDecl of typ * string * sexpr
| SReceive of receive_case list *)

(* type stmt =
  Block of stmt list
| Expr of expr
| Return of expr
| Break
| Continue
| If of expr * stmt * stmt
| For of stmt * expr * expr * stmt
| While of expr * stmt
| Send of string * expr
| SendParent of expr
| Decl of typ * string * expr
| Receive of receive_case list
and receive_case = (pattern * stmt)
and pattern =
  BasePattern of typ * string
| WildcardPattern
| TuplePattern of pattern * pattern *)

  (* let rec check_stmt env stmt =
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in
    (* Decide whether to add environment to expressio *)
    let rec expr = function
        Call (fname, args) as call ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let (et, e') = expr e in
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.ret_type, SCall (fname, args'))
      | IntLit n -> (Int, SIntLit n)
      | FloatLit n -> (Float, SFloatLit n)
      | BoolLit b -> (Bool, SBoolLit b)
      | StringLit s -> (String, SStringLit s)
      | TupleLit (e1, e2) -> raise (TODO "Left as exercise")
      | ArrayLit exps -> raise (TODO "Left as exercise")
      | _ -> raise (TODO "Implement expression handler")
    in
    match stmt with
      | Block stmts ->
          let check_stmt_list (env, acc) s =
            let (extended_env, checked_stmt) = check_stmt env s
            in (extended_env, [acc] @ checked_stmt)
          in let (block_env, checked_stmts) = List.fold_left check_stmt_list
                                                (env, []) stmts
          in (block_env, SBlock checked_stmts)
      | Expr ex -> SExpr (expr ex)
      (* | While
      | Receive rcs -> (Void, SReceive rcs) *)
      | _ -> raise (TODO "Left as exercise") *)

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
         *
         * @TODO - Implement RETURN checking?
         *)
        let rec check_stmt_list envs stmts = match stmts with
              Block _ as block :: stmts ->
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
      | Decl (lt, id, expr) ->
          (match envs with
            env :: _ ->
              if StringMap.mem id env then raise (Failure (id ^ " exists in scope"))
              else let (rt, e') as sexpr = check_expr envs expr in
              (match e' with
                | SNoexpr -> (bind id lt envs, SDecl (lt, id, sexpr))
                | _ -> (bind id lt envs, SDecl (check_assign lt rt expr, id, (lt, e'))))
            | [] -> raise (Failure "Implementation bug: empty environments"))
      | _ -> raise (TODO "Implement other stmt")
  and check_expr (envs: 'a StringMap.t list) (expr: expr) =
    match expr with
      IntLit n -> (Int, SIntLit n)
      | StringLit s -> (String, SStringLit s)
      | FloatLit n -> (Float, SFloatLit n)
      | BoolLit b -> (Bool, SBoolLit b)
      | TupleLit (e1, e2) -> raise (TODO "Implement tuple literal")
      | ArrayLit xs -> raise (TODO "Implement array literal")
      | Call (fname, args) as call ->
        let fd = find_func fname in
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
      | _ -> raise (TODO "Implement expr")

  (* TODO - Add check binds *)
  in let check_function (fdecl: func_decl) =
    { sfname = fdecl.fname; sformals = fdecl.formals; sbody = []; sret_type = fdecl.ret_type }

  in let check_thread (tdecl: thread_decl) =
    let (_, stmts) = check_stmt [StringMap.empty] (Block tdecl.body)
    in match stmts with
      SBlock (sl) -> { stname = tdecl.tname; sbody = sl }
      | _ -> raise (Failure "Failed to parsed thread")

  in (List.map check_thread tdecls, List.map check_function fdecls)
