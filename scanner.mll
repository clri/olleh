{ open Parser }
(*below is placeholder
{ type token = ASSIGN | PLUS | MINUS | TIMES | DIVIDE | SEMI | LESS | GREATER
| MODULO | NOT | OPAREN | CPAREN | OBRACK | CBRACK | OCURLY | CCURLY
| COMMA | DOT | COLON | EQUALS | NEQ | LEQ | GEQ | AND | OR | RET | PRINT | EXIT
| FUNCT | FOR | WHILE | FOREACH | END | IF | ELSE | INTV | CHARV | STRINGV
| BOOLV | VOID | MAPV | LISTV | PLAYER | BOARD | DICT | LETSCO | FRESH | REMOVE
| NULL | LITI of int | VARIABLE of string | LITB of bool | LITS of string
| LITC of char | EOF }*)

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '=' { ASSIGN }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| ';' { SEMI }
| '<' { LESS }
| '>' { GREATER }
| '%' { MODULO }
| '!' { NOT }
| '(' { OPAREN }
| ')' { CPAREN }
| '[' { OBRACK }
| ']' { CBRACK }
| '{' { OCURLY }
| '}' { CCURLY }
| ',' { COMMA }
| '.' { DOT }
| ':' { COLON }
| "==" { EQUALS }
| "!=" { NEQ }
| "<=" { LEQ }
| ">=" { GEQ }
| "&&" { AND }
| "||" { OR }
| "->" { MAPTO }
| "return" { RET }
| "print" { PRINT }
| "exit" { EXIT }
| "function" { FUNCT }
| "for" { FOR }
| "while" { WHILE }
| "foreach" { FOREACH }
| "end" { END }
| "if" { IF }
| "else" { ELSE }
| "int" { INTV }
| "char" { CHARV }
| "String" { STRINGV }
| "boolean" { BOOLV }
| "void" { VOID }
| "map" { MAPV }
| "list" { LISTV }
| "Player" { PLAYER }
| "Board" { BOARD }
| "fresh" { FRESH }
| "remove" { REMOVE }
| "NULL" { NULL }
| "true" { LITB(true) }
| "false" { LITB(false) }
| ['0'-'9']+ as lit { LITI(int_of_string lit) }
| ['\'']['0'-'9' 'a'-'z' 'A'-'Z']['\''] as lit { LITC(lit.[1]) }
| ['a'-'z']['0'-'9' 'a'-'z' 'A'-'Z']+ as lit { VARIABLE(lit) }
| ['"']['0'-'9' 'a'-'z' 'A'-'Z']*['"'] as lit { LITS(lit) }
| "\'\'" { LITC(Char.chr 0) }
| eof { EOF }
| "/*" { comment lexbuf }
| "//" { sincomment lexbuf }
and comment = parse "*/" { tokenize lexbuf }
| _ { comment lexbuf }
and sincomment = parse ['\n'] { tokenize lexbuf }
| _ { sincomment lexbuf }
