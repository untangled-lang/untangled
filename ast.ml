(* 
 * @todo - Think about thread specific types
 *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or  | Mod 

type uop = Neg | Not | Plusplus | Minmin

type typ = Int | Bool | Float | Unit | BigOlType of typ * typ | Thread | String

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

type program = expr
