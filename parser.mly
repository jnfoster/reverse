%{
  open Ast
  open Printf
  open Lexing
%}

%token <int> INT
%token <string> VAR
%token PLUS LPAREN RPAREN LAMBDA DOT LET EQUALS IN EOF

%type <Ast.exp> prog

%start prog
 
%%

prog : exp EOF                     { $1 }

exp : LET VAR EQUALS exp IN exp    { Let($2,$4,$6) }
    | lexp                         { $1 }

lexp : LAMBDA VAR DOT exp         { Lam ($2,$4) }
    | oexp                         { $1 }
 
oexp : oexp PLUS appexp            { Plus($1,$3) }
     | appexp                      { $1 }

appexp : appexp aexp               { App($1,$2) }
    | aexp                         { $1 } 

aexp: VAR                          { Var $1 }
    | INT                          { Int $1 }
    | LPAREN exp RPAREN            { $2 }

