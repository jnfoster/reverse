open Ast
open Machine

(** Finds the location of the variable in the stack,
    returns the stack with the variable removed and the
    former location of the varible if it is found,
    where locations are 1-indexed *)
let find_var_loc (var: var) (s: stack_repr) :
  (int option * stack_repr) =
  let indexed = List.mapi (fun i x -> (i + 1,x)) s in
  List.fold_left (fun (ind, new_st) (i,x) ->
                  match ind, x with
                  | None, Stack_Var w when w = var -> (Some i, new_st)
                  | _ -> (ind, new_st@[x])) (None, []) indexed


(** Compiles an expression to stack machine instructions that grow the stack.
 *  For now, works only on arithmetic expressions. *)
let rec grow_stack : exp -> program * int = function
      | Int i -> ([Push i], 1)
      | Binop (_, e1, e2) -> let (grow_e1, e1_s) = grow_stack e1 in
                             let (grow_e2, e2_s) = grow_stack e2 in
                             (grow_e1 @ grow_e2, e1_s + e2_s)
      | _ -> failwith "TODO"


(** Given the initial stack size, compiles an expression to instructions that 
 * either shrink or keep the stack the same size. Only works on arithmetic 
 * expressions for now. *)
let rec shrink_stack stack_size : exp -> program * int = function
    | Int i -> ([], stack_size)
    | Binop (op, e1, e2) -> let (shrink, size_e2) = shrink_stack stack_size e2 in
                            (* For the case that one sub-expression must be further evaluated
                             * and the other is an integer *)
                            let shrink_e2 = match (shrink, e1, e2) with
                                            | [], Int _, Int _ -> []
                                            | [], _, _ -> [Unroll size_e2]
                                            | _ -> shrink in 
                            let (shrink_e1, size_e1) = shrink_stack size_e2 e1 in
                            let shrink_ops = shrink_e2 @ shrink_e1 in
                            let new_size = size_e1 - 1 in
                            let instr = match op with
                                        | Plus -> Add
                                        | Minus -> Subt
                                        | Divide -> Div
                                        | Multiply -> Mult in
                            (shrink_ops @ [instr; Unroll new_size], new_size)
    | _ -> failwith "TODO"

(** Helper function to to_stack which prepares the stack for a
    Form_Closure instruction by moving free variables referenced
    in the body of the function to the bottom of the stack.
    Explained in further detail in documentation.pdf *)
and place_into_scope (e: exp) orig_stack free_stack local_stack (ops: program) =
  let orig_args = (orig_stack, free_stack, local_stack, ops) in
  let is_in_stack (var:var) (s: stack_repr) =
    List.exists (function
                  | Stack_Var x -> x = var
                 | Const -> failwith "should not be there") s in
  match e with
  | Var v -> if is_in_stack v local_stack || is_in_stack v free_stack
             then orig_args else
               let (loc, new_st) = find_var_loc v orig_stack in
               begin
                 match loc with
                 | None -> raise (UnboundVariable v)
                 | Some loc -> (new_st, (Stack_Var v)::free_stack, local_stack,
                                ops@[Roll loc])
               end
  | Int _ -> orig_args
  | App (e1, e2)
  | Binop (_, e1, e2) ->
     let (os, fs, ls, ops) =
       place_into_scope e1 orig_stack free_stack local_stack ops in
     place_into_scope e2 os fs ls ops
  | Let (v, e1, e2) ->
     let (os, fs, ls, ops) =
       place_into_scope e1 orig_stack
                        free_stack local_stack ops in
     place_into_scope e2 os fs ((Stack_Var v)::ls) ops
  | Lam (v, e) ->
     place_into_scope e orig_stack free_stack ((Stack_Var v)::local_stack) ops


(** Returns the list of stack machine instructions given an expression *)
let translate (init_repr:stack_repr) (e: exp) : program = 
    let (grow_ops, stack_size) = grow_stack e in
    let (shrink_ops, _) = shrink_stack stack_size e in
    grow_ops @ shrink_ops
