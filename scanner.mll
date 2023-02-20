{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
  | "/*" { comment_scanner lexbuf }
  | ';' { SEMI }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | ',' { COMMA }

  | '+'      { PLUS }
  | "++"     { PLUSPLUS }
  | '-'      { MINUS }
  | "--"     { MINUSMINUS }
  | '*'      { TIMES }
  | '/'      { DIVIDE }
  | '%'      { MOD }
  | '='      { ASSIGN }
  | "+="     { PLUSASSIGN }
  | "-="     { MINUSASSIGN }
  | "*="     { TIMESASSIGN }
  | "/="     { DIVIDEASSIGN }
  | "%="     { MODASSIGN }
  | "=="     { EQ }
  | "!="     { NEQ }
  | '<'      { LT }
  | "<="     { LEQ }
  | ">"      { GT }
  | ">="     { GEQ }
  | "&&"     { AND }
  | "||"     { OR }
  | "!"      { NOT }

  (* Thread specific tokens *)
  | "->"         { ARROW }
  | "<<"         { SEND }
  | "receive"    { RECEIVE }
  | "parent"     { PARENT }
  | "thread_def" { THREAD_DEF }
  | "thread"     { THREAD }
  | "spawn"      { SPAWN }

  (* Function-specific tokens *)
  | "return" { RETURN }

  (* Conditions *)
  | "if"     { IF }
  | "else"   { ELSE }
  | "for"    { FOR }
  | "while"  { WHILE }
  | "break"  { BREAK }
  | "continue" { CONTINUE }

  (* Types *)
  | "int" { INT }
  | "bool" { BOOL }
  | "float" { FLOAT }
  | "string" { STRING }
  | "thread" { THREAD }
  | "true" { BLIT(true) }
  | "false" { BLIT(false) }
  | "sem"  { SEMAPHORE }
  | "void" { VOID }
  | "_"  { WCARD }

  | digits as lxm { ILIT(int_of_string lxm) }
  | digits '.'  digit* as lxm { FLIT(lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | '"' { string_scanner [] lexbuf }

  | eof { EOF }
  | _ as other_char { raise (Failure("illegal character " ^ Char.escaped other_char)) }


and comment_scanner = parse
  "*/" { token lexbuf }
  | _ { comment_scanner lexbuf }

and string_scanner s_list = parse
  '"' { SLIT(String.concat "" (List.rev s_list)) }
  (* '\\' (escaped_key as c) { TODO: interpret the next character as an escape character () } *)
  | _ as new_char { string_scanner ((String.make 1 new_char) :: s_list) lexbuf }
