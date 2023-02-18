open Ast

let () =
  let lex_buf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lex_buf in Ast.string_of_program ast
