open Ast
open Machine 

let () =
  (* (1) get file name from command-line arguments *)
  let _ =
    if Array.length Sys.argv < 2 then
      (Format.printf "Usage: rev <file>\n";
       exit 0) in
  
  (* (2) Check command line for arguments to function, 
   * build stack representation based on user input *)
  let init_stack = 
    let args = Array.mapi (fun i x -> (x, i+1)) Sys.argv in
    Array.fold_left (fun stack (x, pos) ->
      if pos > 2 then 
        let parsed = 
          try int_of_string x
          with Failure "int_of_string" -> 
            Format.printf "Argument to function must be an integer\n";
            exit 0 in
        (Int parsed)::stack
      else stack) [] args in

  let _ =
    Format.printf "Initial stack:";
    Pprint.print_stack init_stack;
    Format.print_newline () in 


  (* (3) parse file to an expression *)
  let file = open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel file in
  let parsed_exp =
    try
      Parser.prog Lexer.token lexbuf
    with Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Format.printf "Syntax error at line %d\n" pos.Lexing.pos_lnum;
      exit 1 in


  (* (4) Pretty print the expression *)
  let _ =
    Format.printf "@[";
    Format.printf "Expression:@\n  @[";
    Pprint.print_exp parsed_exp;
    Format.printf "@]@\n@\n" in

  (* (5) If there are command line arguments, build the expression 
   * that applies these arguments to the input function *)
  let (e, init_stack_repr, _) = 
    List.fold_right (fun input (e, repr, input_num) ->
      let var = "user_input" ^ (string_of_int input_num) in
      let e' = App(e, Var var) in
      let repr' = Stack_Var var::repr in
      (e', repr', input_num + 1)) init_stack (parsed_exp, [], 1) in

  (* (6) Translate the expression  to machine instructions *)
  let program = Translation.translate init_stack_repr e in
  let _ =
    Format.printf "Translated expression into machine instructions...@\n";
    Format.print_flush ();
    Pprint.print_instrs program in

  (* (7 Pretty print the resulting value *)
  let init_state = (program, init_stack, [], []) in
  let (_, _, _, history_prog) as result_state = Eval.eval_stack init_state in
  let v = match result_state with
    | (_, [h], _, _) -> h
    | _ -> failwith "unused variable on stack" in
  let _ =
    Format.printf "\n Result:@\n  @[";
    Pprint.print_stack_val v;
    Format.printf "@]@\n";
    Format.printf "@]\n" in

  let _ = 
    Format.printf "Program history...@\n";
    Format.print_flush ();
    Pprint.print_instrs history_prog in

  (* (8) Reverse the computation and confirm that the original state is
   * restored. Prints the original state. *)
  let (_, restored_stack, _, _) as restored_state = 
    Format.printf "\nReversing history...@\n";
    Eval.reverse_history result_state in
  let _ = 
    Format.printf "Restored stack:";
    Pprint.print_stack restored_stack;
    Format.print_newline ();
    assert (restored_state = init_state) in 
  ()
