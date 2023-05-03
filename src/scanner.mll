{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  (* Whitespace *)
  [' ' '\t' '\r' '\n'] { token lexbuf }
  (* Comments *)
  | "/*" { block_comment_scanner lexbuf }
  | "//" { line_comment_scanner lexbuf }

  (* start keywords *)

  (* Punctuation *)
  | ';' { SEMI }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | ',' { COMMA }

  (* Operators *)
  | '+'      { PLUS }
  | "++"     { PLUSPLUS }
  | '-'      { MINUS }
  | "--"     { MINUSMINUS }
  | '*'      { TIMES }
  | '/'      { DIVIDE }
  | '%'      { MOD }
  | "**"     { POW }
  | '='      { ASSIGN }
  | "+="     { PLUSASSIGN }
  | "-="     { MINUSASSIGN }
  | "*="     { TIMESASSIGN }
  | "/="     { DIVIDEASSIGN }
  | "%="     { MODASSIGN }
  | "**="    { POWASSIGN }
  | "=="     { EQ }
  | "!="     { NEQ }
  | '<'      { LT }
  | "<="     { LEQ }
  | ">"      { GT }
  | ">="     { GEQ }
  | "&&"     { AND }
  | "||"     { OR }
  | "!"      { NOT }

  (* Thread-specific tokens *)
  | "->"         { ARROW }
  | "<<"         { SEND }
  | "receive"    { RECEIVE }
  | "thread_def" { THREAD_DEF }
  | "thread"     { THREAD }
  | "spawn"      { SPAWN }

  (* Function-specific tokens *)
  | "return" { RETURN }

  (* Control flow *)
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
  | "semaphore" { SEMAPHORE }
  | "void" { VOID }

  | "true" { BLIT(true) }
  | "false" { BLIT(false) }
  | "_"  { WCARD }

  (* end keywords *)

  | digits as lxm { ILIT(int_of_string lxm) }
  | digits '.' digit* as lxm { FLIT(lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | '"' { string_scanner [] lexbuf }

  | eof { EOF }
  | _ as other_char { raise (Failure("illegal character " ^ Char.escaped other_char)) }


and block_comment_scanner = parse
  "*/" { token lexbuf }
  | _ { block_comment_scanner lexbuf }

and line_comment_scanner = parse
  '\n' { token lexbuf }
  | _ { line_comment_scanner lexbuf }

and string_scanner s_list = parse
  '"' { SLIT(String.concat "" (List.rev s_list)) }
  | '\\' (_ as c) {
    if c = '\n' then string_scanner s_list lexbuf (* Ignore escaped newlines in strings *)
    else string_scanner ((String.make 1 (
      match c with
        | 'n' -> '\010'
        | 'r' -> '\013'
        | 'b' -> '\008'
        | 't' -> '\009'
        | '\\' -> '\\'
        | '"' -> '"'
        | _   -> raise (Failure("Unrecognized escape character \\" ^ Char.escaped c))
    )) :: s_list) lexbuf
  }
  | _ as new_char { string_scanner ((String.make 1 new_char) :: s_list) lexbuf }
