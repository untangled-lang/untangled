open Ast

let rec eval ast = 
  match ast with
  (vdecls, tdecls) -> 
    List.iter (fun (_, v) -> print_string v) vdecls 
  (* | _ -> print_string "Something else" *)


let () =
  let lex_buf = Lexing.from_channel stdin in
  (* let ast = Parser.program Scanner.token lex_buf in eval ast *)
  let ast = Parser.program Scanner.token lex_buf in Ast.string_of_program ast
