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

  (* (4) Compile expression *)
  let instr = Compiler.compile e in
  let _ =
    Format.printf "Compiled expression...@\n";
    Format.print_flush ();
    Pprint.print_instrs instr in

  (* (5) Pretty print the final value *)
  let v = match Machine.eval_stack [] instr with
    | [h] -> h
    | _ -> failwith "nonlinear" in
  let _ =
    Format.printf "\n Result:@\n  @[";
    Pprint.print_stack_val v;
    Format.printf "@]@\n";
    Format.printf "@]" in
  ()
