%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE ASSIGN
%token LBRACKET RBRACKET PLUSPLUS MINUSMINUS MOD
%token NOT EQ NEQ LT LEQ GT GEQ AND OR VLINE
// Conditions
%token IF ELSE WHILE FOR BREAK
%token SEMAPHORE

// Types
%token INT BOOL FLOAT SEM WCARD
%token <int> ILIT
%token <bool> BLIT
%token <string> ID 
%token <string> FLIT
%token <string> SLIT

// Built-in Functions
%token EXIT PRINT

// Thread specific tokens
%token ARROW SEND GET RECEIVE PARENT THREAD_DEF THREAD SPAWN

// EOF
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS PLUSPLUS MINUSMINUS
%left TIMES DIVIDE MOD
%right NOT

%%

program:
  decls EOF { $1 }

decls:
  /* nothing */ { ([], []) }
  | decls vdecl { (($2 :: fst $1), snd $1) }
  | decls tdecl { (fst $1, ($2 :: snd $1)) }

vdecl:
  typ ID SEMI { ($1, $2) }

typ:
  INT { Int }
  | BOOL { Bool }
  | FLOAT { Float } 
  | LPAREN typ COMMA typ RPAREN { Tuple($2, $4) }

expr:
  ILIT          { Literal($1)            }
  | FLIT	     { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }

expr_opt:
  /* nothing */ { Noexpr }
  | expr          { $1 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

// TODO
stmt:
  expr SEMI                                 { Expr $1               }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

tdecl: THREAD_DEF ID LBRACE stmt_list RBRACE
    { { tname = $2; body = $4; } }


// receive: RECEIVE LBRACE pattern_list RBRACE {
//   (* TODO: *)
// }
