
open Ast

type sexpr = typ * sx
and sx =
  SIntLit of int
| SFloatLit of string
| SBoolLit of bool
| SStringLit of string
| STupleLit of sexpr * sexpr
| SArrayLit of sexpr list
| SId of string
| SBinop of sexpr * op * sexpr
| SPreUnop of unop * sexpr
| SPostUnop of unop * sexpr
| SAssign of string * sexpr
| SAssignIndex of sexpr * sexpr * sexpr
| SSpawn of string
| SCall of string * sexpr list
| SIndex of sexpr * sexpr
| SNoexpr

type sstmt =
  SBlock of sstmt list
| SExpr of sexpr
| SReturn of sexpr
| SBreak
| SContinue
| SIf of sexpr * sstmt * sstmt
| SFor of sstmt * sexpr * sexpr * sstmt
| SWhile of sexpr * sstmt
| SSend of sexpr * sexpr
| SDecl of sdecl_type
| SReceive of receive_case list
and sdecl_type =
| SBaseDecl of typ * string * sexpr
| STupleDecl of sdecl_type * sdecl_type * sexpr
and receive_case = (pattern * sstmt)
and pattern =
  SBasePattern of typ * string
| SWildcardPattern
| STuplePattern of pattern * pattern

type sthread_decl = {
  stname: string;
  sbody: sstmt list;
}

type sfunc_decl = {
  sfname: string;
  sformals: bind list;
  sbody: sstmt list;
  sret_type: typ;
}

type sprogram = sthread_decl list * sfunc_decl list


(* Printing *)
let indent s =
  let lines = String.split_on_char '\n' s in
  let indented_lines = List.map
    (fun x -> (if String.length (String.trim x) == 0 then "" else "  " ^ x))
    lines
  in
  String.concat "\n" indented_lines



let rec string_of_unop unop e =
  match unop with
    Neg -> "-" ^ string_of_sexpr e
  | Not -> "!" ^ string_of_sexpr e
  | Plusplus -> string_of_sexpr e ^ "++"
  | Minmin -> string_of_sexpr e ^ "--"

and string_of_sexpr (t, expr) =
  "(" ^ string_of_typ t ^ " : " ^
    (match expr with
      SIntLit(n) -> Int.to_string n
    | SFloatLit(n) -> n
    | SBoolLit(b) -> Bool.to_string b
    | SStringLit(s) -> "\"" ^ s ^ "\""
    | STupleLit(t1, t2) -> "(" ^ string_of_sexpr t1 ^ ", " ^ string_of_sexpr t2 ^ ")"
    | SArrayLit(n) -> "[" ^ String.concat ", " (List.map string_of_sexpr n) ^ "]"
    | SId(s) -> s
    | SBinop (e1, o, e2) -> "(" ^ string_of_sexpr e1 ^ string_of_op o ^ string_of_sexpr e2 ^ ")"
    | SPreUnop (unop, e) -> "(" ^ string_of_unop unop e ^ ")"
    | SPostUnop (unop, e) -> "(" ^ string_of_unop unop e ^ ")"
    | SAssign(s, e) -> "(" ^ s ^ " = " ^ string_of_sexpr e ^ ")"
    | SSpawn(s) -> "spawn " ^ s
    | SCall(s, es) ->
      (let expressionStrings = List.map string_of_sexpr es in
      s ^ "(" ^ String.concat ", " expressionStrings ^ ")")
    | SNoexpr -> ""
    | SIndex(s, e) -> string_of_sexpr s ^ "[" ^ string_of_sexpr e ^ "]"
    | SAssignIndex(s, e1, e2) -> string_of_sexpr s ^ "[" ^ string_of_sexpr e1 ^ "] = " ^ string_of_sexpr e2)
  ^ ")"


let rec string_of_spattern pat =
  match pat with
    SBasePattern(t, s) -> string_of_typ t ^ " " ^ s
  | SWildcardPattern -> "_"
  | STuplePattern(p1, p2) -> "(" ^ string_of_spattern p1 ^ ", " ^ string_of_spattern p2 ^ ")"

let rec string_of_stuple = function
  | STupleDecl(t1, t2, (_, SNoexpr)) -> "(" ^ string_of_stuple t1 ^ ", " ^ string_of_stuple t2 ^ ")"
  | STupleDecl(t1, t2, sexpr) -> "(" ^ string_of_stuple t1 ^ ", " ^ string_of_stuple t2 ^ ")" ^ " = " ^ string_of_sexpr sexpr
  | SBaseDecl(t, s, _) -> string_of_typ t ^ " " ^ s


let rec string_of_sstmt statement =
  match statement with
    SBlock(stmts) ->
      "{\n" ^ indent (String.concat "\n" (List.map string_of_sstmt stmts)) ^ "\n}"
  | SExpr(expr) -> string_of_sexpr expr ^ ";";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";"
  | SBreak -> "break;"
  | SContinue -> "continue;"
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s1 ^
                      " else " ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sstmt e1 ^ " " ^ string_of_sexpr e2 ^ "; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SSend(s, e) -> (string_of_sexpr s) ^ " << " ^ (string_of_sexpr e) ^ ";"
  | SDecl(SBaseDecl (t, id, (_, SNoexpr))) -> string_of_typ t ^ " " ^ id ^ ";"
  | SDecl(SBaseDecl (t, id, expr)) -> string_of_typ t ^ " " ^ id ^ " = " ^ (string_of_sexpr expr) ^ ";"
  | SDecl (STupleDecl _ as tup) -> string_of_stuple tup ^ ";"
  | SReceive(cases) -> "receive {\n" ^ indent (String.concat "\n" (List.map
      (fun (pat, stmt) -> ((string_of_spattern pat) ^ " -> " ^ (string_of_sstmt stmt)))
    cases)) ^ "\n}"


let string_of_sthread_decl thread =
  "thread_def " ^ thread.stname ^ match thread.sbody with
    | [] -> " {}"
    | _ -> " {\n" ^ indent (String.concat "\n" (List.map string_of_sstmt thread.sbody)) ^ "\n}"


let string_of_sfunc_decl func =
  string_of_typ func.sret_type ^ " " ^ func.sfname ^ "(" ^
  String.concat ", " (List.map string_of_bind func.sformals) ^ ") {\n" ^
  indent (String.concat "\n" (List.map string_of_sstmt func.sbody)) ^ "\n}"


let string_of_sprogram (thread_decls, function_decls) = String.concat "\n\n" (List.filter (fun x -> x <> "") [
  String.concat "\n\n" (List.map string_of_sthread_decl thread_decls);
  String.concat "\n\n" (List.map string_of_sfunc_decl function_decls)
]) ^ "\n"
