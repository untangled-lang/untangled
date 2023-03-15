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
  let void_t = L.void_type context in
  let the_module = L.create_module context "Untangled" in

  (* TODO - Add type *)
  let print_t : L.lltype = L.function_type void_t [||] in
  let print_func : L.llvalue = L.declare_function "print" print_t the_module in

  (* TODO - Extend map with extra information? For example, message passing? *)
  let thread_decls =
    let thread_decl m tdecl =
      let stname = tdecl.stname and
          ttype = L.function_type void_t [||] in
      StringMap.add stname (L.define_function tdecl.stname ttype the_module, tdecl) m in
    List.fold_left thread_decl StringMap.empty tdecls in

  let build_thread_body tdecl =
    let (the_thread, _) = StringMap.find tdecl.stname thread_decls in
    let builder = L.builder_at_end context (L.entry_block the_thread) in
    let string_format_str = L.build_global_stringptr "%s" "fmt" builder in

    let rec expr (builder: L.llbuilder) ((_, sexpr : sexpr)) =
      match sexpr with
        | SStringLit s -> L.const_stringz context s
        | SCall ("print", [sexpr]) ->
            L.build_call print_func [| string_format_str; (expr builder sexpr) |] "print" builder
        | _ -> raise (Failure "Implement other exprs builder")
    and stmt (builder: L.llbuilder) = function
      (* TODO - Update SBlock to account for scoping rules *)
      SBlock sblock -> List.fold_left stmt builder sblock
      | SExpr sexpr -> let _ = expr builder sexpr in builder
      | _ -> builder

    in stmt builder (SBlock tdecl.sbody)

  in let _ = List.map build_thread_body tdecls in the_module