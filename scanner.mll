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
| "Null" { NULL }
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
| "true" { LITB(true) }
| "false" { LITB(false) }
| "main" { raise Not_found }
| ['0'-'9']+ as lit { LITI(int_of_string lit) }
| ['\'']['0'-'9' 'a'-'z' 'A'-'Z']['\''] as lit { LITC(lit.[1]) }
| ['a'-'z']['0'-'9' 'a'-'z' 'A'-'Z' '_']* as lit { VARIABLE(lit) }
| "\'\'" { LITC(Char.chr 0) }
| ['"'][^ '"']*['"'] as lit { LITS(String.sub lit 1 (String.length lit - 2)) }
| eof { EOF }
| "/*" { comment lexbuf }
| "//" { sincomment lexbuf }
and comment = parse "*/" { tokenize lexbuf }
| _ { comment lexbuf }
and sincomment = parse ['\n'] { tokenize lexbuf }
| _ { sincomment lexbuf }
