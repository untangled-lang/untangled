%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA
%token PLUS MINUS TIMES DIVIDE MOD POW
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MODASSIGN POWASSIGN
%token NOT EQ NEQ LT LEQ GT GEQ AND OR PLUSPLUS MINUSMINUS
%token IF ELSE WHILE FOR BREAK CONTINUE

// Types
%token INT BOOL FLOAT WCARD STRING VOID
%token <int> ILIT
%token <bool> BLIT
%token <string> ID
%token <string> FLIT
%token <string> SLIT

// Function-specific tokens
%token RETURN

// Thread-specific tokens
%token ARROW SEND RECEIVE THREAD_DEF THREAD SEMAPHORE SPAWN

// EOF
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MODASSIGN POWASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left POW
%nonassoc NOT
%nonassoc PLUSPLUS MINUSMINUS
%%

program:
  decls EOF { List.rev (fst $1), List.rev (snd $1) }

decls:
  { ([], []) }
  | decls thread_decl { (($2 :: fst $1), snd $1) }
  | decls func_decl { (fst $1, ($2 :: snd $1)) }

typ:
    VOID { Void }
  | INT { Int }
  | BOOL { Bool }
  | FLOAT { Float }
  | LPAREN typ COMMA typ RPAREN { Tuple($2, $4) }
  | STRING { String }
  | THREAD { Thread }
  | SEMAPHORE { Semaphore }
  | typ LBRACKET ILIT RBRACKET { Array($1, $3) }

expr:
  // Literals
    ILIT             { IntLit($1)             }
  | FLIT             { FloatLit($1)           }
  | BLIT             { BoolLit($1)            }
  | SLIT             { StringLit($1)          }
  | LPAREN expr COMMA expr RPAREN { TupleLit($2, $4) }
  | LBRACKET array_elements RBRACKET { ArrayLit(List.rev $2) }
  // TODO - Double check about our semantics for this
  | LPAREN RPAREN    { Unit }
  // Names
  | ID               { Id($1)                 }
  // Binary operators
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr MOD    expr { Binop($1, Mod,   $3)   }
  | expr POW    expr { Binop($1, Pow,   $3)   }
  | expr EQ     expr { Binop($1, Equality, $3)}
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  // Unary operators
  | MINUS expr %prec NOT { PrefixUnop(Neg, $2)      }
  | NOT expr         { PrefixUnop(Not, $2)          }
  | ID PLUSPLUS    { PostfixUnop(Plusplus, Id($1))     }
  | ID MINUSMINUS  { PostfixUnop(Minmin, Id($1))       }
  // Assignment
  | ID ASSIGN expr   { Assign($1, $3)         }
  | ID PLUSASSIGN expr { Assign($1, Binop(Id($1), Add, $3)) }
  | ID MINUSASSIGN expr { Assign($1, Binop(Id($1), Sub, $3)) }
  | ID TIMESASSIGN expr { Assign($1, Binop(Id($1), Mult, $3)) }
  | ID DIVIDEASSIGN expr { Assign($1, Binop(Id($1), Div, $3)) }
  | ID MODASSIGN expr { Assign($1, Binop(Id($1), Mod, $3)) }
  | ID POWASSIGN expr { Assign($1, Binop(Id($1), Pow, $3)) }
  // Array access (reading)
  | ID LBRACKET expr RBRACKET { Index($1, $3) }
  // Array access (writing)
  | ID LBRACKET expr RBRACKET ASSIGN expr { AssignIndex($1, $3, $6) }
  | ID LBRACKET expr RBRACKET PLUSASSIGN expr { AssignIndex($1, $3, Binop(Index($1, $3), Add, $6)) }
  | ID LBRACKET expr RBRACKET MINUSASSIGN expr { AssignIndex($1, $3, Binop(Index($1, $3), Sub, $6)) }
  | ID LBRACKET expr RBRACKET TIMESASSIGN expr { AssignIndex($1, $3, Binop(Index($1, $3), Mult, $6)) }
  | ID LBRACKET expr RBRACKET DIVIDEASSIGN expr { AssignIndex($1, $3, Binop(Index($1, $3), Div, $6)) }
  | ID LBRACKET expr RBRACKET MODASSIGN expr { AssignIndex($1, $3, Binop(Index($1, $3), Mod, $6)) }
  | ID LBRACKET expr RBRACKET POWASSIGN expr { AssignIndex($1, $3, Binop(Index($1, $3), Pow, $6)) }
  // Grouping (parentheses)
  | LPAREN expr RPAREN { $2                   }
  // `spawn` keyword
  | SPAWN ID         { Spawn($2)              }
  // Function calls
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }


array_elements:
  /* nothing */ { [] }
  | expr        { [$1] }
  | array_elements COMMA expr { $3 :: $1 }

args_opt:
  /* nothing */ { [] }
  | args_list   { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

expr_opt:
  /* nothing */   { Noexpr }
  | expr          { $1 }

for_init_statement:
  vdecl { $1 }
  | expr_opt SEMI { Expr($1) }

stmt:
  expr SEMI                                 { Expr($1)              }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | vdecl                                   { $1                    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN for_init_statement expr SEMI expr_opt RPAREN stmt
                                            { For($3, $4, $6, $8)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }
  | RETURN expr_opt SEMI                    { Return($2)            }
  | BREAK SEMI                              { Break                 }
  | CONTINUE SEMI                           { Continue              }
  | ID SEND expr SEMI                       { Send($1, $3)          }
  | receive                                 { $1                    }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

pattern:
  | typ ID                                 { BasePattern($1, $2) }
  | WCARD                                  { WildcardPattern }
  | LPAREN pattern COMMA pattern RPAREN    { TuplePattern($2, $4) }

receive_case:
  pattern ARROW stmt { ($1, $3) }

receive_cases:
  /* nothing */ { [] }
  | receive_cases receive_case { $2 :: $1 }

receive: RECEIVE LBRACE receive_cases RBRACE { Receive(List.rev $3) }

// TODO: syntactic sugar for multiple variable declarations? e.g. `int x, y = 2, z;`
vdecl:
    typ ID SEMI                 { Decl($1, $2, Noexpr)  }
  | typ ID ASSIGN expr SEMI     { Decl($1, $2, $4)      }

thread_decl: THREAD_DEF ID LBRACE stmt_list RBRACE
    { { tname = $2; body = List.rev $4; } }

func_decl: typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
    { { fname = $2; formals = List.rev $4; body = List.rev $7; ret_type = $1; } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }
