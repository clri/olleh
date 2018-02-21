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
    | Rem of string * expr
    | Assignm of string * string * expr
    | Newtobj of typ * typ
    | Newobj of typ * expr list
    | Call of string * expr list
    | Noexpr

type stmt =
      Expr of expr
    | Return of expr
    | Print of expr
    | If of expr * (stmt list) * (stmt list)
    | While of expr * (stmt list)
    | For of expr * (stmt list)
    | Foreach of string * expr * (stmt list)
    | Bind of typ * string
    | Assignd of typ * string * expr
    | Exit of int

type func_decl = {
    typ         : typ;
    fname       : string;
    formals     : formalbind list;
    body        : stmt list;
}

type program = stmt list * func_decl list




(**)
