open Ast

let () = 
  (* (1) get file name from command-line arguments *)
  let _ = 
    if Array.length Sys.argv <> 2 then
      (Format.printf "Usage: rev <file>\n";
       exit 0) in 

  (* (2) parse file to an expression *)
  let file = open_in (Sys.argv.(1)) in 
  let lexbuf = Lexing.from_channel file in 
  let e = 
    try 
      Parser.prog Lexer.token lexbuf
    with Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Format.printf "Syntax error at line %d\n" pos.Lexing.pos_lnum;
      exit 1 in 

  (* (3) Pretty print the expression *)
  let _ =
    Format.printf "@[";
    Format.printf "Expression:@\n  @[";
    Pprint.print_exp e;
    Format.printf "@]@\n@\n" in 

  (* (4) Evaluate expression *)
  let _ = 
    Format.printf "Evaluating the expression...@\n@\n";
    Format.print_flush () in 

  let v = Eval.eval empty e in 

  (* (5) Pretty print the final value *)
  let _ =
    Format.printf "Result:@\n  @[";
    Pprint.print_val v;
    Format.printf "@]@\n";
    Format.printf "@]" in
  ()
