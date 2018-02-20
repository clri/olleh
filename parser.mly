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
  decls EOF { $1 }

decls:
    /* nothing */ { ([], [])		}
  | decls vdecl { (($2 :: fst $1), snd $1) }
  | decls fdecl { (fst $1, ($2 :: snd $1)) }


fdecl:
  FUNCT typ VARIABLE OPAREN formals_opt CPAREN COLON vdecl_list stmt_list END SEMI
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
  | VOID    { void  }
  | STRINGV { String}
  | MAPV    { map }
  | LISTV   { list }
  | PLAYER  { Player }
  | BOARD   { Board }

vdecl_list:
    /* nothing */ { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
  typ LITS { ( $1, $2) }

stmt_list:
    /* nothing */ { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  SEMI { ; }
