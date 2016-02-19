{
open Parser
open Printf
exception Eof
exception Err

let incline lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }
}

let digit = ['0'-'9']
let id = ['a'-'z'] ['a'-'z' '0'-'9']*
let ws = [' ' '\t']

rule token = parse
| ws      { token lexbuf }
| '\n'    { incline lexbuf; token lexbuf }
| "+"     { PLUS }
| "/"     { DIVIDE }
| "-"     { MINUS }
| "*"     { MULTIPLY }
| "("     { LPAREN }
| ")"     { RPAREN }
| "."     { DOT }
| "="     { EQUALS }
| "lambda" { LAMBDA }
| "let"   { LET }
| "in"    { IN } 
| id as v { VAR(v) }
| digit+ as n { INT(int_of_string n) }
| eof     { EOF }

| _ as c  { 
            let pos = lexbuf.Lexing.lex_curr_p in
            printf "Error at line %d\n" pos.Lexing.pos_lnum;
            printf "Unrecognized character: [%c]\n" c;
            exit 1 
          }
