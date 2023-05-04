type op =
    Add
  | Sub
  | Mult
  | Div
  | Mod
  | Pow
  | Equality
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or

type unop = Neg | Not | Plusplus | Minmin

type typ =
    Void
  | Bool
  | Int
  | Float
  | String
  | Thread
  | Semaphore
  | Tuple of typ * typ
  | Array of typ * int

type bind = typ * string

type expr =
  IntLit of int
| FloatLit of string
| BoolLit of bool
| StringLit of string
| TupleLit of expr * expr
| ArrayLit of expr list
| Id of string
| Binop of expr * op * expr
| PrefixUnop of unop * expr
| PostfixUnop of unop * expr
| Assign of string * expr
| AssignIndex of expr * expr * expr
| Spawn of string
| Call of string * expr list
| Index of expr * expr
| Noexpr

type stmt =
  Block of stmt list
| Expr of expr
| Return of expr
| Break
| Continue
| If of expr * stmt * stmt
| For of stmt * expr * expr * stmt
| While of expr * stmt
| Send of string * expr
| Decl of decl_type
| Receive of receive_case list
and decl_type =
| BaseDecl of typ * string * expr
| TupleDecl of decl_type * decl_type * expr
and receive_case = (pattern * stmt)
and pattern =
  BasePattern of typ * string
| WildcardPattern
| TuplePattern of pattern * pattern


type thread_decl = {
  tname: string;
  body: stmt list;
}

type func_decl = {
  fname: string;
  formals: bind list;
  body: stmt list;
  ret_type: typ;
}

type program = thread_decl list * func_decl list



(***** PRETTY PRINTING *****)


(* Utils *)
let indent s =
  let lines = String.split_on_char '\n' s in
  let indented_lines = List.map
    (fun x -> (if String.length (String.trim x) == 0 then "" else "  " ^ x))
    lines
  in
  String.concat "\n" indented_lines



let rec string_of_typ t =
  match t with
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | Tuple (t1, t2) -> ("(" ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2) ^ ")")
  | Thread -> "thread"
  | Semaphore -> "semaphore"
  | String -> "string"
  | Array (t, size) -> string_of_typ t ^ "[" ^ Int.to_string size ^ "]"


let string_of_op op = match op with
  | Add -> " + "
  | Sub -> " - "
  | Mult -> " * "
  | Div -> " / "
  | Mod -> " % "
  | Pow -> " ** "
  | Equality -> " == "
  | Neq -> " != "
  | Less -> " < "
  | Leq -> " <= "
  | Greater -> " > "
  | Geq -> " >= "
  | And -> " && "
  | Or -> " || "


let rec string_of_unop unop e =
  match unop with
    Neg -> "-" ^ string_of_expr e
  | Not -> "!" ^ string_of_expr e
  | Plusplus -> string_of_expr e ^ "++"
  | Minmin -> string_of_expr e ^ "--"


and string_of_expr expr =
  match expr with
    IntLit(n) -> Int.to_string n
  | FloatLit(n) -> n
  | BoolLit(b) -> Bool.to_string b
  | StringLit(s) -> "\"" ^ s ^ "\""
  | TupleLit(t1, t2) -> "(" ^ string_of_expr t1 ^ ", " ^ string_of_expr t2 ^ ")"
  | ArrayLit(n) -> "[" ^ String.concat ", " (List.map string_of_expr n) ^ "]"
  | Id(s) -> s
  | Binop (e1, o, e2) -> "(" ^ string_of_expr e1 ^ string_of_op o ^ string_of_expr e2 ^ ")"
  | PrefixUnop (unop, e) -> "(" ^ string_of_unop unop e ^ ")"
  | PostfixUnop (unop, e) -> "(" ^ string_of_unop unop e ^ ")"
  | Assign(s, e) -> "(" ^ s ^ " = " ^ string_of_expr e ^ ")"
  | Spawn(s) -> "spawn " ^ s
  | Call(s, es) ->
    (let expressionStrings = List.map string_of_expr es in
    s ^ "(" ^ String.concat ", " expressionStrings ^ ")")
  | Noexpr -> ""
  | Index(s, e) -> string_of_expr s ^ "[" ^ string_of_expr e ^ "]"
  | AssignIndex(s, e1, e2) -> string_of_expr s ^ "[" ^ string_of_expr e1 ^ "] = " ^ string_of_expr e2


let rec string_of_pattern pat =
  match pat with
    BasePattern(t, s) -> string_of_typ t ^ " " ^ s
  | WildcardPattern -> "_"
  | TuplePattern(p1, p2) -> "(" ^ string_of_pattern p1 ^ ", " ^ string_of_pattern p2 ^ ")"

let rec string_of_tuple = function
  | TupleDecl(t1, t2, Noexpr) -> "(" ^ string_of_tuple t1 ^ ", " ^ string_of_tuple t2 ^ ")"
  | TupleDecl(t1, t2, e) -> "(" ^ string_of_tuple t1 ^ ", " ^ string_of_tuple t2 ^ ")" ^ " = " ^ (string_of_expr e)
  | BaseDecl(t, s, _) -> string_of_typ t ^ " " ^ s


let rec string_of_stmt statement =
  match statement with
    Block(stmts) ->
      "{\n" ^ indent (String.concat "\n" (List.map string_of_stmt stmts)) ^ "\n}"
  | Expr(expr) -> string_of_expr expr ^ ";";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";"
  | Break -> "break;"
  | Continue -> "continue;"
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ") " ^ string_of_stmt s1 ^
                      " else " ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_stmt e1 ^ " " ^ string_of_expr e2 ^ "; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Send(s, e) -> s ^ " << " ^ (string_of_expr e) ^ ";"
  | Decl(BaseDecl (t, id, Noexpr)) -> string_of_typ t ^ " " ^ id ^ ";"
  | Decl(BaseDecl (t, id, expr)) -> string_of_typ t ^ " " ^ id ^ " = " ^ (string_of_expr expr) ^ ";"
  | Decl(TupleDecl _ as tup) -> string_of_tuple tup ^ ";"
  | Receive(cases) -> "receive {\n" ^ indent (String.concat "\n" (List.map
      (fun (pat, stmt) -> ((string_of_pattern pat) ^ " -> " ^ (string_of_stmt stmt)))
    cases)) ^ "\n}"


let string_of_thread_decl thread =
  "thread_def " ^ thread.tname ^ match thread.body with
    | [] -> " {}"
    | _ -> " {\n" ^ indent (String.concat "\n" (List.map string_of_stmt thread.body)) ^ "\n}"


let string_of_bind (t, id) = string_of_typ t ^ " " ^ id

let string_of_func_decl func =
  string_of_typ func.ret_type ^ " " ^ func.fname ^ "(" ^
  String.concat ", " (List.map string_of_bind func.formals) ^ ") {\n" ^
  indent (String.concat "\n" (List.map string_of_stmt func.body)) ^ "\n}"


let string_of_program (thread_decls, function_decls) = String.concat "\n\n" (List.filter (fun x -> x <> "") [
  String.concat "\n\n" (List.map string_of_thread_decl thread_decls);
  String.concat "\n\n" (List.map string_of_func_decl function_decls)
]) ^ "\n"
