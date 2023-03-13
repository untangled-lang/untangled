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

let translate sast =
  let context = L.global_context () in
  let the_module = L.create_module context "Untangled" in the_module