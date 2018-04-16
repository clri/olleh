(* AST and functions for printing it.
Adapted from MicroC ast.ml and ast.ml in
class notes. *)

type op = Add | Sub | Mult | Div | Equal | Mod |
        Neq | Less | Leq | Greater | Geq |
        And | Or

type uop = Neg | Not

type typ = Int | Bool | Char | String | List | Map | Player | Board | Void

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
    | Board -> "Board"
    | Void -> "Void"

let string_of_expr _ = "EXPR"
let string_of_uop _ = "UOP"
let string_of_op x = match x with 
      Add -> "Add"
    | Sub -> "Sub"
    | _ -> "OP"



(**)
