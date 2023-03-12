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
  (* Note, the kind argument is just to provide better error messages *)
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
    in List.fold_left add_bind StringMap.empty [("print", String, Void)] in (* TODO: - Add more builtin functions *)
  
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined" 
      and dup_err = "duplicate function " ^ fd.fname
      and make_err er = raise (Failure er)
      and n = fd.fname
    in match fd with
      _ when StringMap.mem n fmap -> make_err built_in_err
    | _ when StringMap.mem n fmap -> make_err dup_err
    | _ -> StringMap.add n fd fmap
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

  let rec check_stmt env stmt =
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
      | TupleLit (e1, e2) ->
          let (e1_env, sexps) = expr e1
          
          (* raise (TODO "Left as exercise") *)
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
      | _ -> raise (TODO "Left as exercise")

  in let rec check_function fdecl = check_stmt StringMap.empty env fdecl.body

  in let rec check_thread tdecl = check_stmt StringMap.empty env tdecl.body

  in (List.map check_thread thread_defs, List.map check_function function_decls)

  