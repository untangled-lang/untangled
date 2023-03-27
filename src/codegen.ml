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

let translate ((tdecls : sthread_decl list), (fdecls : sfunc_decl list)) =
  let context = L.global_context () in
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type context in
  let the_module = L.create_module context "Untangled" in

  (* TODO - Add type *)
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
     L.declare_function "printf" printf_t the_module in
  let print_t : L.lltype = L.function_type void_t [||] in
  let print_func : L.llvalue = L.declare_function "print" print_t the_module in

  (* TODO - Extend map with extra information? For example, message passing? *)
  let thread_decls =
    let thread_decl m tdecl =
      let stname = tdecl.stname and
          ttype = L.function_type void_t [||] in
      StringMap.add stname (L.define_function (if tdecl.stname = "Main" then "main" else tdecl.stname) ttype the_module, tdecl) m in
    List.fold_left thread_decl StringMap.empty tdecls in

  let build_thread_body tdecl =
    let (the_thread, _) = StringMap.find tdecl.stname thread_decls in
    let builder = L.builder_at_end context (L.entry_block the_thread) in
    let string_format_str = L.build_global_stringptr "%s" "fmt" builder in

    let rec expr (builder: L.llbuilder) ((_, sexpr : sexpr)) =
      match sexpr with
        (* | SStringLit s -> L.const_stringz context s *)
        | SStringLit s -> L.build_global_stringptr s "tmp" builder
        | SCall ("print", [sexpr]) ->
            L.build_call printf_func [| string_format_str; (expr builder sexpr) |] "printf" builder
        | _ -> raise (Failure "Implement other exprs builder")
    and stmt (builder: L.llbuilder) = function
      (* TODO - Update SBlock to account for scoping rules *)
      SBlock sblock -> List.fold_left stmt builder sblock
      | SExpr sexpr -> let _ = expr builder sexpr in builder
      | _ -> builder
    in
    let builder = stmt builder (SBlock tdecl.sbody) in
    L.build_ret_void builder

  in let _ = List.map build_thread_body tdecls in the_module
