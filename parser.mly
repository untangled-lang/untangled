%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE ASSIGN
%token LBRACKET RBRACKET PLUSPLUS MINUSMINUS MOD 
%token NOT EQ NEQ LT LEQ GT GEQ AND OR VLINE
// Conditions
%token IF ELSE WHILE FOR BREAK

// Types
%token INT BOOL FLOAT SEM WCARD 
%token <int> ILIT
%token <bool> BLIT
%token <string> ID FLIT SLIT

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