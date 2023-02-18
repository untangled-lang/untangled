(*
 * @todo - Think about thread specific types
 *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Mod

type uop = Neg | Not | Plusplus | Minmin

type typ = Int | Bool | Float | Unit | Tuple of typ * typ | Thread | String

type bind = typ * string

type expr =
Literal of int
| Fliteral of string
| BoolLit of bool
| Id of string
| Binop of expr * op * expr
| Unop of uop * expr
| Assign of string * expr
| Call of string * expr list
| Noexpr

type stmt =
  Block of stmt list
| Expr of expr
| Return of expr
| If of expr * stmt * stmt
| For of expr * expr * expr * stmt
| While of expr * stmt
(* TODO: Add message sent / receive statements *)

(*
 * Threads definition:
 *    name
 * .  message sent/receive?
 *    body: statement?
 *)
type thread_decl = {
  tname: string;
  body: stmt list;
  (* TODO: Keep track of message sent / receive? *)
}

type program = bind list * thread_decl list
(* type program = bind *)

let rec string_of_typ t =
  match t with
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Unit -> "unit"
  | Tuple (t1, t2) -> ("(" ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2) ^ ")")
  | Thread -> "thread"
  | String -> "string"

(* Int | Bool | Float | Unit | Tuple of typ * typ | Thread | String *)

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

(* This is from MicroC *)
(* let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n" *)

let string_of_program (vars, funcs) =
  print_string (String.concat "" (List.map string_of_vdecl vars))
  (* String.concat "\n" (List.map string_of_fdecl funcs) *)