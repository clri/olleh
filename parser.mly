%{ open Ast %}
%token SEMI COLON OPAREN CPAREN OBRACK CBRACK OCURLY CCURLY COMMA PLUS MINUS TIMES DIVIDE MODULO DOT ASSIGN FUNCT FRESH MAPTO PRINT NULL
%token INTV CHARV STRINGV BOOLV VOID MAPV LISTV PLAYER ASCIV
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

%left COMMA
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQUALS NEQ
%left LESS GREATER LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right NOT NEG ASCIV
%nonassoc PRINT

%%

program:
  pgm { $1 }

/*synthesize global statements/declarations into the "main" function*/
pgm:
  decls EOF { ((fst (fst $1)),
                { typ = Void;
                fname = "main";
                formals = [];
                body = ( (Expr (Call("InitializeRandom",[])))
                                :: List.rev (snd (fst $1))) }
                        :: snd $1) }

/*((global vars, global stmts), functs)--keep globals separate
so we can allocate space for them. */
decls:
   /* nothing */ { ( ([], []), [])               }
  | decls stmt { ( (fst (fst $1), ($2 :: snd (fst $1)) ), snd $1) }
  | decls fdecl { (fst $1, ($2 :: snd $1)) }
  | decls vdecl { ( ($2 :: (fst (fst $1)), ($2 :: snd (fst $1)) ), snd $1) }
  | decls dastmt { ( ( fst $2 :: (fst (fst $1)),
                       (snd $2 :: (fst $2 :: snd (fst $1))) ),
                     snd $1) }

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

typ:
    typr { $1 }
  | obj  { $1 }

typr:
    INTV    { Int }
  | BOOLV   { Bool }
  | CHARV   { Char }
  | VOID    { Void }
  | STRINGV { String }
  | LISTV LESS LISTV GREATER { Listlist }
  | LISTV LESS CHARV GREATER { Charlist }

obj:
    MAPV LESS CHARV GREATER { Charmap }
  | MAPV LESS STRINGV GREATER { Stringmap }
  | PLAYER  { Player }

stmt_list:
    /* nothing */ { [] }
  | stmt_list stmt { $2 :: $1 }
  | stmt_list vdecl { $2 :: $1 }
  | stmt_list rstmt { $2 :: $1 }
  | stmt_list dastmt { snd $2 :: fst $2 :: $1 }

/*for return statements. had been implemented separately from regular statements
 due to a deprecated feature*/
rstmt:
   RET expr_opt SEMI { Return $2  }

/*declare/assign statement, gets partitioned into two*/
dastmt:
     typ VARIABLE ASSIGN expr SEMI { ( Bind($1, $2), Expr (Assign ($2, $4)) ) }

vdecl:
      typ VARIABLE SEMI { Bind( $1, $2) }

stmt:
  expr SEMI                                 { Expr $1               }
  | PRINT expr SEMI { Print($2) }
  | ifs END SEMI                            { $1 }
  | FOR OPAREN expr CPAREN COLON stmt_list END SEMI { For($3, $6)   }
  | WHILE OPAREN expr CPAREN COLON stmt_list END SEMI
                                             { While($3, $6)         }
  | FOREACH VARIABLE expr COLON stmt_list END SEMI
                                             { Foreach($2, $3, $5)   }
  | EXIT SEMI                               { Exit(0) }


ifs:
   IF OPAREN expr CPAREN COLON stmt_list %prec NOELSE
                                            { If($3, $6, [])        }
   | IF OPAREN expr CPAREN COLON stmt_list ELSE COLON stmt_list
                                            { If($3, $6, $9)        }

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
  | expr MAPTO expr { [($1, $3)] }
  | maplis COMMA expr MAPTO expr { ($3, $5) :: $1 }

expr:
    LITI             { Literali($1)                        }
  | LITC	     { Literalc($1)                        }
  | LITB             { Literalb($1)                        }
  | LITS             { Literals($1)                        }
  | ASCIV expr       { Unop(Asc, $2)                       }
  | NULL             { Null                                }
  | VARIABLE         { Variable($1)                        }
  | VARIABLE DOT VARIABLE { Vmember($1, $3)                } /*member of obj*/
  | OBRACK lis CBRACK { Literall( List.rev $2 )            } /*list literal*/
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
  | VARIABLE DOT VARIABLE OPAREN args_opt CPAREN { Callm($1, $3, $5) }
  | VARIABLE OPAREN args_opt CPAREN { Call($1, $3)         }
  | FRESH MAPV OPAREN STRINGV CPAREN { Newtobj( Stringmap )        }
  | FRESH MAPV OPAREN CHARV CPAREN { Newtobj( Charmap )        }
  | FRESH PLAYER OCURLY maplis CCURLY   { Newobj( Player, $4 )    }
  | FRESH LISTV OPAREN expr CPAREN { Newlis(Charlist, ($4 :: [])) }
  | FRESH LISTV OPAREN expr COMMA expr CPAREN { Newlis(Listlist, ($4 :: ($6 :: []))) }
  | OPAREN expr CPAREN { $2                                }














/**/
