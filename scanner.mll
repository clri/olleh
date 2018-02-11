{ open Parser }

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
| ':' { COLON }
| '(' { OPAREN }
| ')' { CPAREN }
| '[' { OBRACK }
| ']' { CBRACK }
| '{' { OCURLY }
| '}' { CCURLY }
| '.' { DOT }
| "==" { EQUALS }
| "!=" { NEQ }
| "<=" { LEQ }
| ">=" { GEQ }
| "&&" { AND }
| "||" { OR }
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
| "dictionary" { DICT }
| "letterScores" { LETSCO }
| "fresh" { FRESH }
| "remove" { REMOVE }
| "NULL" { NULL }
| "{}" { MLIT(empty) } /*needed? hw to MLIT--map lit*/
| ['0'-'9']+ as lit { ILIT(int_of_string lit) } /*MLIT. BLIT*/
| ['a'-'z']+['0'-'9' a'-'z' 'A'-'Z']+ as lit { VARIABLE(lit) }
| "true" { BLIT(true) }
| "false" { BLIT(false) }
| eof { EOF }
| "/*" { comment lexbuf }
| "//" { sincomment lexbuf }
and comment = parse "*/" { tokenize lexbuf }
| _ { comment lexbuf }
and sincomment = parse ['\n'] { tokenize lexbuf }
| _ { sincomment lexbuf }

/*todo: test, how to do string literal*/
