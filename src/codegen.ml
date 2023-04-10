(*
 * TODO:
 *    Creating the string type
 *    Declare the print function
 *    Define the print function
 *    Fill in the body of the print function
 *    Construct the code for a function call expression
 *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let deep_copy_stringmap map =
  let new_map = StringMap.empty in
  let new_map = StringMap.fold (fun k v acc -> StringMap.add k v acc) map new_map in
  new_map

let translate ((tdecls : sthread_decl list), (fdecls : sfunc_decl list)) =
  let context = L.global_context () in
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type context
  and pointer_t = L.pointer_type (L.i8_type context) in
  let the_module = L.create_module context "Untangled" in

  (* TODO - Add type *)

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* pthread_t is an opaque struct which is a pointer *)
  let pthread_create_t : L.lltype =
    L.function_type i32_t [| pointer_t; pointer_t; pointer_t; pointer_t |] in
  let pthread_create_func : L.llvalue =
    L.declare_function "pthread_create" pthread_create_t the_module in

  let pthread_join_t : L.lltype =
    L.function_type i32_t [| i8_t; pointer_t |] in
  let pthread_join_func : L.llvalue =
    L.declare_function "pthread_join" pthread_join_t the_module in

  let strlen_t : L.lltype =
    L.function_type i32_t [| L.pointer_type i8_t |] in
  let strlen_func : L.llvalue =
    L.declare_function "strlen" strlen_t the_module in

  let strcat_t : L.lltype =
    L.function_type (L.pointer_type i8_t) [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  let strcat_func : L.llvalue =
    L.declare_function "strcat" strcat_t the_module in

  (* Build instructions to concatenate two strings. Matches call signature of functions like L.build_add *)
  let build_strcat x y name builder =
    let xlength = L.build_call strlen_func [| x |] "strlen_x" builder and
        ylength = L.build_call strlen_func [| y |] "strlen_y" builder in
    (* let _ = L.build_call printf_func [| L.build_global_stringptr "%d\n" "fmt" builder ; xlength |] "print x length" builder *)
    let combined_length = L.build_add xlength ylength "new_length" builder in
    let combined_length_with_null = L.build_add combined_length (L.const_int i32_t 1) "new_length" builder in
    (* Allocate space for our concatenated string *)
    let new_string = L.build_array_alloca i8_t combined_length_with_null "allocated_combined_string" builder in
    (* Set the first place in our new string to the null characters *)
    let first_element = L.build_gep new_string [| (L.const_int i8_t 0) |] "first_element" builder in
    let _ = L.build_store (L.const_int i8_t 0) first_element builder in
    (* Call strcat for x *)
    let _ = L.build_call strcat_func [| new_string; x |] "strcat_x" builder in
    (* Call strcat for y *)
    L.build_call strcat_func [| new_string; y |] name builder in

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder)
  in
  (* TODO - Extend map with extra information? For example, message passing? *)
  let thread_decls =
    let thread_decl m tdecl =
      let stname = tdecl.stname and
          ttype = L.function_type (L.pointer_type i8_t) [| L.pointer_type i8_t |] in
      (* Add paramaters for threads *)
      StringMap.add stname (L.define_function (if tdecl.stname = "Main" then "main" else tdecl.stname) ttype the_module, tdecl) m in
    List.fold_left thread_decl StringMap.empty tdecls in

  let build_thread_body tdecl =
    let (the_thread, _) = StringMap.find tdecl.stname thread_decls in
    let builder = L.builder_at_end context (L.entry_block the_thread) in
    let string_format_str = L.build_global_stringptr "%s" "fmt" builder in
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let rec expr ((builder: L.llbuilder), env) ((_, sexpr : sexpr)) =
      match sexpr with
        | SIntLit i -> (L.const_int i32_t i, env)
        | SStringLit s -> (L.build_global_stringptr s "tmp" builder, env)
        | SBoolLit b -> (L.const_int i1_t (if b then 1 else 0), env)
        | SFloatLit l -> (L.const_float_of_string float_t l, env)
        | SNoexpr -> (L.const_int i32_t 0, env)
        | SId s -> (L.build_load (StringMap.find s env) s builder, env)
        | SCall ("print", [sexpr]) ->
            let (llvalue, env') =  expr (builder, env) sexpr in
              (L.build_call printf_func [| string_format_str; llvalue |] "printf" builder, env')
        | SCall ("string_of_float", [sexpr]) ->
            let (llvalue, env') = expr (builder, env) sexpr in
              (match L.float_of_const llvalue with
                Some v -> (L.build_global_stringptr (string_of_float v) "tmp" builder, env')
                | None -> raise (Failure "Bug in parsing float expression"))
        | SBinop (e1, op, e2) ->
            let (t1, _) = e1 in
            let (e1', env') = expr (builder, env) e1 in
            let (e2', env'') = expr (builder, env') e2 in
            let op =
              (match t1 with
                A.Float ->
                  (match op with
                      A.Add              -> L.build_fadd
                    | A.Sub              -> L.build_fsub
                    | A.Mult             -> L.build_fmul
                    | A.Div              -> L.build_fdiv
                    | A.Equality         -> L.build_fcmp L.Fcmp.Oeq
                    | A.Pow              -> raise (Failure "Implement power on float")
                    | A.Neq              -> L.build_fcmp L.Fcmp.One
                    | A.Less             -> L.build_fcmp L.Fcmp.Olt
                    | A.Leq              -> L.build_fcmp L.Fcmp.Ole
                    | A.Greater          -> L.build_fcmp L.Fcmp.Ogt
                    | A.Geq              -> L.build_fcmp L.Fcmp.Oge
                    | A.And | A.Or | Mod ->
                        raise (Failure "internal error: semant should have rejected
                                        and/or/mod on float"))
                | A.Int ->
                  (match op with
                    | A.Add       -> L.build_add
                    | A.Sub       -> L.build_sub
                    | A.Mult      -> L.build_mul
                    | A.Div       -> L.build_sdiv
                    | A.And       -> L.build_and
                    | A.Mod       -> L.build_srem
                    | A.Or        -> L.build_or
                    | A.Equality  -> L.build_icmp L.Icmp.Eq
                    | A.Neq       -> L.build_icmp L.Icmp.Ne
                    | A.Less      -> L.build_icmp L.Icmp.Slt
                    | A.Leq       -> L.build_icmp L.Icmp.Sle
                    | A.Greater   -> L.build_icmp L.Icmp.Sgt
                    | A.Geq       -> L.build_icmp L.Icmp.Sge
                    | A.Pow       -> raise (Failure "Implement power on int"))
                | A.String ->
                  (match op with
                    | A.Add -> build_strcat
                    | _ -> raise (Failure "Operation not supported on string arguments"))
                | _ -> raise (Failure "Implement other")) in
            (op e1' e2' "binop_result" builder, env'')
          | SSpawn tn ->
              let (thread, _) = StringMap.find tn thread_decls in
              (* Allocate &pthread_t *)
              let id = L.build_alloca (L.pointer_type i8_t) "id" builder in
              let _ = L.set_alignment 8 id in
              let _ = L.build_call pthread_create_func [| id; (L.const_null i8_t); thread; (L.const_null i8_t)|] "create" builder in
              (* Load the value of *pthread_t *)
              let id_value = L.build_load id "pthread_t value" builder in
              (* Wait for the thread to complete *)
              (L.build_call pthread_join_func [| id_value; (L.const_null i8_t)|] "join" builder, env)

        (* | SSpawn s ->  *)
            (* let llvalue = Llvm.const_int (Llvm.i64_type context) 1 in
            let value = Int64.to_int (Llvm.int64_of_const llvalue) in
            (llvalue, env') *)
            (* Format is typ value *)
            (* TODO - Ask richard if this is gucci :) *)
            (* let ocamlvalue = String.split_on_char ' ' (L.string_of_llvalue llvalue)
            in
            let stringP = L.build_global_stringptr (List.hd (List.rev ocamlvalue)) "tmp" builder
              in (stringP, env') *)
          (* let format_string_of (acc: string) ((ty, _ : sexpr)) = *)
            (* acc ^ " " ^ (match ty with
              | Void -> ""
              | Bool ->  raise (Failure "Unimplemented")
              | Int ->  "%d"
              | Float ->  raise (Failure "Unimplemented")
              | String ->  "%s"
              | Thread ->  raise (Failure "Unimplemented")
              | Semaphore ->  raise (Failure "Unimplemented")
              | Tuple (t1, t2) ->  raise (Failure "Unimplemented")
              | Array (arrayType, count) ->  raise (Failure "Unimplemented"))
          in let format_string = List.fold_left format_string_of "" printArgs and
                 llvalues = List.map (fun arg -> expr builder arg) printArgs in
          let printf_arg_arr = Array.append [| L.build_global_stringptr format_string "fmt" builder |] (Array.of_list llvalues)
          in L.build_call printf_func printf_arg_arr "printf" builder *)
          (* let printFormatString = build_format_string printFormatString in
          let printArgs = List.map  sexprs in *)
          (* L.build_call printf_func [| string_format_str; (expr builder sexpr) |] "printf" builder *)
        (* | SAssign varname sx -> *)
      | _ -> raise (Failure "Implement other exprs builder")
    and stmt ((builder: L.llbuilder), env) sstmt =
      match sstmt with
        (* TODO - Update SBlock to account for scoping rules *)
          SBlock sblock ->
            let _ = List.fold_left stmt (builder, env) sblock in (builder, env)
        | SExpr sexpr -> let (_, env2) = expr (builder, env) sexpr in (builder, env2)
        | SDecl (ty, var_name, sx) -> let (llvalue, env2) = expr (builder, env) sx in
                                  let alloca = L.build_alloca (match ty with
                                    | Void -> void_t
                                    | Bool -> i1_t
                                    | Int -> i32_t
                                    | Float -> float_t
                                    | String -> L.pointer_type i8_t
                                    | Thread -> void_t
                                    | Semaphore -> void_t
                                    | Tuple (t1, t2) -> void_t
                                    | Array (arrayType, count) -> void_t) var_name builder in
                                  let _ = L.build_store llvalue alloca builder in
                                  (builder, StringMap.add var_name alloca env2)
        | _ -> (builder, env)
    in
    (* thread function follows pthread function type and returns a NULL pointer *)
    let (builder, _) = stmt (builder, StringMap.empty) (SBlock tdecl.sbody) in
                        (L.build_ret (L.const_null (L.pointer_type i8_t)) builder)

  in let _ = List.map build_thread_body tdecls in the_module
