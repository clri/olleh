open Ast
module StringMap = Map.Make(String)

(*NOTE: FOR NOW THIS DOES NOTHING, INCLUDES CALC CODE SO WE CAN MODIFY IT TO
*GENERATE SOME OUTPUT INSTEAD OF INTERPRETING COMMANDS*)
(*before we add, change to print some representation of the tokens*)
let rec printast tkns =
   print_newline (*
      Lit(x) -> Printf.printf "Lit %d " x
     | Var(x) -> eval assig (StringMap.find x assig)
     | Asn(var, valu) -> let (realv, asns) = eval assig valu
      in (realv, (StringMap.add var valu asns))
     | Seq(e1, e2) -> let (_, asns) = eval assig e1 in eval asns e2
     | Binop(e1, op, e2) ->
      let (v1, _) = (eval assig e1) and (v2, _) = (eval assig e2) in
      match op with
    	Add -> (v1 + v2, assig)
      | Sub -> (v1 - v2, assig)
      | Mul -> (v1 * v2, assig)
      | Div -> (v1 / v2, assig)*)

let () =
  let lex_buf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lex_buf in
  let _ = eval printast expr in
  print_newline
