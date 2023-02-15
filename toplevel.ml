open Ast

let rec eval ast = 
  match ast with
  | Literal(i) -> string_of_int i
  | _ -> "something else"


let () =
  let lex_buf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lex_buf in
  let test = eval ast in
  print_endline(test);
