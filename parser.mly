%{
  open Ast
  open Printf
  open Lexing
%}

%token <int> INT
%token <string> VAR
%token PLUS DIVIDE MULTIPLY MINUS 
%token LPAREN RPAREN 
%token LAMBDA DOT LET EQUALS IN 
%token EOF

%start prog
%type <Ast.exp> prog
%%

prog : exp EOF                     { $1 }

exp : LET VAR EQUALS exp IN exp    { Let($2,$4,$6) }
    | lexp                         { $1 }

lexp : LAMBDA VAR DOT exp          { Lam ($2,$4) }
    | pmexp                        { $1 }

pmexp : pmexp PLUS mdexp           { Binop (Plus,$1,$3) }
    | pmexp MINUS mdexp            { Binop (Minus,$1,$3) }
    | mdexp                        { $1 }

mdexp : mdexp MULTIPLY appexp      { Binop (Multiply,$1,$3) }
    | mdexp DIVIDE appexp          { Binop (Divide,$1,$3) }
    | appexp                       { $1 }

appexp : appexp aexp               { App($1,$2) }
    | aexp                         { $1 } 

aexp: VAR                          { Var $1 }
    | INT                          { Int $1 }
    | LPAREN exp RPAREN            { $2 }

