open Ast

let () =
  let lex_buf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lex_buf
  in let res = Ast.string_of_program ast
  in print_endline res
