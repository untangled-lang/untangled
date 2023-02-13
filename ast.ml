(* 
 * @todo - Think about thread specific types
 *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or  | Mod 

type uop = Neg | Not | Plusplus | Minmin

type typ = Int | Bool | Float | Unit | typ * typ | Thread | String

type bind = typ * string

