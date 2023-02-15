{ open Parser }

let digit = ['0' - '9']
let char = ['a'-'z' 'A'-'Z']

let digits = digit+
let chars = char+
let string = char*
(* let ascii = [ -~] *)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
  | "/*" { comment lexbuf }
  | ';' { SEMI }

  | "int" { INT }
  | "bool" { BOOL }
  | "float" { FLOAT }
  | "true" { BLIT(true) }
  | "false" { BLIT(false) }
  
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
  | _ { comment lexbuf }

(* rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| "++"     { PLUSPLUS }
| '-'      { MINUS }
| "--"     { MINUSMINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "|"      { VLINE }

(* Thread specific tokens *)
| "->"     	  { ARROW }
| "<<"         { SEND }
| ">>"         { GET }
| "receive"    { RECEIVE }
| "parent"     { PARENT }
| "thread_def" { THREAD_DEF }
| "thread"     { THREAD }
| "spawn"      { SPAWN }

(* Conditions *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "break"  { BREAK }

(* Built-in functions *)
| "exit"  { EXIT }
| "print"  { PRINT }

(* Types *)
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "true"   { BLIT(true) }
| "false"  { BLIT(false) }
| "sem"  { SEMAPHORE }
| "_"  { WCARD }

| digits as lxm { ILIT(int_of_string lxm) }
| digits '.'  digit* as lxm { FLIT(lxm) }
| string as lxm { SLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf } *)
