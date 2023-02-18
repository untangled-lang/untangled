(*
 * @todo - Think about thread specific types
 *)

type id = string;

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Mod

type uop = Neg | Not | Plusplus | Minmin

type typ = Unit | Bool | Int | Float | String | Thread | Tuple of typ * typ | Array of typ * int

type bind = typ * string

type expr =
  IntLit of int
| FloatLit of string
| BoolLit of bool
| StringLit of string
| Id of string
| Binop of expr * op * expr
| Unop of uop * expr
| Assign of string * expr 
| Spawn of string
| Noexpr

type stmt =
  Block of stmt list 
| Expr of expr
| If of expr * stmt * stmt
| For of expr * expr * expr * stmt
| While of expr * stmt
(* TODO: | Receive of (typ * bind list * stmt) list *)
| Send of thread_id * expr
| Decl of typ * string * expr
(* Note: “and” can be used for mutually recursive definitions https://stackoverflow.com/a/29471895/4414003 *)

type thread_decl = {
  tname: string;
  body: stmt list;
}

type program = bind list * thread_decl list

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