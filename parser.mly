%{ open Ast %}
%token SEMI COLON OPAREN CPAREN OBRACK CBRACK OCURLY CCURLY COMMA NULL PLUS MINUS TIMES DIVIDE MODULO DOT ASSIGN FUNCT FRESH MAPTO PRINT REMOVE
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
%left TIMES DIVIDE MODULO
%right NOT NEG

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
  | decls stmt { (($2 :: fst $1), snd $1) }
  | decls fdecl { (fst $1, ($2 :: snd $1)) }


typ:
    INTV    { Int }
  | BOOLV   { Bool }
  | CHARV   { Char }
  | VOID    { Void }
  | STRINGV { String }
  | LISTV   { List }
  | obj     { $1 }

obj:
    MAPV    { Map }
  | PLAYER  { Player }
  | BOARD   { Board }


fdecl:
  FUNCT typ VARIABLE OPAREN formals_opt CPAREN COLON stmt_list END SEMI
     { { typ = $2;
         fname = $3;
         formals = $5;
         body = List.rev $8 } }


formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ VARIABLE                   { [($1,$2)]     }
  | formal_list COMMA typ VARIABLE { ($3,$4) :: $1 }


stmt_list:
    /* nothing */ { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  | expr SEMI                               { Expr $1               }
  | RET expr_opt SEMI                       { Return $2             }
  | IF OPAREN expr CPAREN COLON stmt_list %prec NOELSE END SEMI
                                            { If($3, $6, [])        }
  | IF OPAREN expr CPAREN COLON stmt_list ELSE COLON stmt_list END SEMI
                                            { If($3, $6, $9)        }
  | WHILE OPAREN expr CPAREN stmt_list END SEMI
                                            { While($3, $5)         }
  | FOR CPAREN expr OPAREN stmt_list END    { For($3, $5)           }
  | FOREACH VARIABLE VARIABLE COLON stmt_list END SEMI
                                            { Foreach($2, $3, $5)   }
  | typ VARIABLE SEMI { Bind( $1, $2) }
  | typ VARIABLE ASSIGN expr { Assignd( $1, $2, $4) }
  | obj VARIABLE ASSIGN FRESH obj OPAREN args_opt CPAREN SEMI { Assignf( $1, $2, $5, $7 )}
  | PRINT expr { Print($2) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

lis:
    /* nothing */ { [] }
  | expr { [$1] }
  | lis COMMA expr { $3 :: $1 }

maplis:
    /* nothing */ { [] }
  | expr MAPTO expr { [($1, $3)] } /*how to store?*/
  | maplis COMMA expr MAPTO expr { ($3, $5) :: $1 }

expr:
    LITI             { Literali($1)                        }
  | LITC	     { Literalc($1)                        }
  | LITB             { Literalb($1)                        }
  | LITS             { Literals($1)                        }
  | VARIABLE         { Variable($1)                        }
  | VARIABLE DOT VARIABLE { Vmember($1, $3)                } /*member of obj*/
  | OBRACK lis CBRACK { Literall($2)                       } /*list literal*/
  | OCURLY maplis CCURLY { Literalm($2)                    } /*map literal*/
  | expr PLUS   expr { Binop($1, Add,   $3)                }
  | expr MINUS  expr { Binop($1, Sub,   $3)                }
  | expr TIMES  expr { Binop($1, Mult,  $3)                }
  | expr DIVIDE expr { Binop($1, Div,   $3)                }
  | expr MODULO expr { Binop($1, Mod,   $3)                }
  | expr EQUALS expr { Binop($1, Equal, $3)                }
  | expr NEQ    expr { Binop($1, Neq,   $3)                }
  | expr LESS   expr { Binop($1, Less,  $3)                }
  | expr LEQ    expr { Binop($1, Leq,   $3)                }
  | expr GREATER expr { Binop($1, Greater, $3)             }
  | expr GEQ    expr { Binop($1, Geq,   $3)                }
  | expr AND    expr { Binop($1, And,   $3)                }
  | expr OR     expr { Binop($1, Or,    $3)                }
  | MINUS expr %prec NEG { Unop(Neg, $2)                   }
  | NOT expr         { Unop(Not, $2)                       }
  | VARIABLE ASSIGN expr { Assign($1, $3)                  }
  | VARIABLE DOT VARIABLE ASSIGN expr { Assignm($1, $3, $5)} /*assign to mem*/
  | VARIABLE OPAREN args_opt CPAREN { Call($1, $3)         }
  | VARIABLE DOT REMOVE OPAREN expr CPAREN { Rem($1, $5)   }
  | OPAREN expr CPAREN { $2                                }














/**/
