/* %{ open Ast %} later */
%token SEMI COLON OPAREN CPAREN OBRACK CBRACK OCURLY CCURLY COMMA NULL PLUS MINUS TIMES DIVIDE MODULO DOT ASSIGN FUNCT
%token INTV CHARV STRINGV BOOLV VOID BOARD MAPV LISTV PLAYER
%token EQUALS LESS GREATER NOT NEQ LEQ GEQ AND OR
%token RET END IF ELSE FOR FOREACH WHILE
%token <int> LITI
%token <bool> LITB
%token <string> LITS
%token <string> VARIABLE
%token <char> LITC
%token EXIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQUALS NEQ
%left LESS GREATER LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%%

program:
  stmt_list EOF { $1 }

decls:
    /* nothing */ { ([], [])		}
  | decls vdecl { (($2 :: fst $1), snd $1) }
  | decls fdecl { (fst $1, ($2 :: snd $1)) }


fdecl:
  FUNCT typ VARIABLE OPAREN formals_opt CPAREN COLON stmt_list END SEMI
     { { typ = $2;
         fname = $3;
         formals = $5;
         locals = List.rev $8;
         body = List.rev $9 } }


formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ VARIABLE                   { [($1,$2)]     }
  | formal_list COMMA typ VARIABLE { ($3,$4) :: $1 }

typ:
    INTV    { int   }
  | BOOLV   { boolean }
  | CHARV   { char }
  | VOID    { void }
  | STRINGV { String }
  | LISTV   { list }
  | obj     { $1 }

obj:
    MAPV    { map }
  | PLAYER  { Player }
  | BOARD   { Board }

vdecl_list:
    /* nothing */ { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ VARIABLE SEMI { ( $1, $2) }
  | typ VARIABLE ASSIGN expr { ( $1, $2, $4) }
  | obj VARIABLE ASSIGN FRESH obj RPAREN fsh LPAREN SEMI { ( $2, $2, $5, $7 )}

fsh:
    /* nothing */ { [] }
  | typ  { $1 :: [] } /*you can have a map of any type for now*/
  | expr COMMA expr { $2 :: $1 } /*correct order?*/

stmt_list:
    /* nothing */ { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    vdecl_list
  | expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | IF OPAREN expr CPAREN COLON stmt_list %prec NOELSE END SEMI
                                            { If($3, $6, [])        }
  | IF OPAREN expr CPAREN COLON stmt_list ELSE COLON stmt_list END SEMI
                                            { If($3, $6, $9)        }
  | WHILE OPAREN expr CPAREN stmt_list END SEMI
                                            { While($3, $5)         }
  | FOR LPAREN expr RPAREN stmt_list END    { For($3, $5, $7, $9)   }
  | FOREACH VARIABLE VARIABLE COLON stmt_list END SEMI
                                            { Foreach($2, $3, $5)   }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }















/**/
