(* AST and functions for printing it.
Adapted from MicroC ast.ml and ast.ml in
class notes. *)

type op = Add | Sub | Mult | Div | Equal | Mod |
        Neq | Less | Leq | Greater | Geq |
        And | Or

type uop = Neg | Not

type typ = Int | Bool | Char | String | List | Map | Player | Void
           | Stringmap | Charmap | Charlist | Listlist

type formalbind = typ * string

type expr =
      Literali of int
    | Literalc of char
    | Literals of string
    | Literalb of bool
    | Null
    | Variable of string
    | Vmember of string * string
    | Literall of expr list
    | Literalm of (expr * expr) list
    | Binop of expr * op * expr
    | Unop of uop * expr
    | Assign of string * expr
    | Assignm of string * string * expr
    | Newtobj of typ * typ
    | Newobj of typ * (expr * expr) list
    | Newlis of typ * expr list
    | Call of string * expr list
    | Callm of string * string * expr list
    | Noexpr

type stmt =
      Expr of expr
    | Return of expr
    | Print of expr
    | If of expr * (stmt list) * (stmt list)
    | While of expr * (stmt list)
    | For of expr * (stmt list)
    | Foreach of string * expr * (stmt list)
    | Exit of int
    | Bind of typ * string

type func_decl = {
    typ         : typ;
    fname       : string;
    formals     : formalbind list;
    body        : stmt list;
}

type program = stmt list * func_decl list




(*@TODO: implement printing functions better*)
let string_of_typ x = match x with
      String -> "String"
    | Int -> "Int"
    | List -> "List"
    | Map -> "Map"
    | Bool -> "Bool"
    | Char -> "Char"
    | Player -> "Player"
    | Void -> "Void"
    | Stringmap -> "Map<string>"
    | Charmap -> "Map<char>"
    | Charlist -> "List<char>"
    | Listlist -> "List<list>"

let string_of_uop x = match x with
      Not -> "Not"
    | Neg -> "Negative"

let string_of_op x = match x with
      Add -> "Plus"
    | Sub -> "Minus"
    | Mult -> "Times"
    | Div -> "Divided by"
    | Equal -> "Equals"
    | Mod -> "Modulo"
    | Neq -> "Not Equal To"
    | Less -> "Less Than"
    | Leq -> "Less Than or Equal To"
    | Greater -> "Greater Than"
    | Geq -> "Greater Than or Equal To"
    | And -> "And"
    | Or -> "Or"

let rec string_of_expr e = match e with
      Literali x -> string_of_int x
    | Literalc c -> Printf.sprintf "%c" c
    | Literals s -> s
    | Literalb b -> string_of_bool b
    | Null -> "Null"
    | Variable v -> v
    | Vmember (v, m) -> v ^ "." ^ m
    | Literall l -> "[...]"
    | Literalm m -> "{...}"
    | Binop (e1, op, e2) ->
        (string_of_expr e1) ^ " " ^ (string_of_op op) ^ " " ^ (string_of_expr e2)
    | Unop (op, e1) -> (string_of_uop op) ^ " " ^ (string_of_expr e1)
    | Assign (v, e1) -> v ^ " = " ^ (string_of_expr e1)
    | Assignm (v, m, e1) -> v ^ "." ^ m ^ " = " ^ (string_of_expr e1)
    | Newtobj (t1, t2) -> "fresh " ^ (string_of_typ t1) ^ "(" ^ (string_of_typ t2) ^ ")"
    | Newobj (t, elis) -> "fresh " ^ (string_of_typ t) ^ "[...]"
    | Call (s, elis) -> s ^ "(...)"
    | Callm (s, m, elis) -> s ^ "." ^ m ^ "(...)"
    | Noexpr -> ""
