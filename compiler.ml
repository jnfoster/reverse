open Ast
open Machine

(** Used to maintain a representation of the stack *)
type stack_repr = Const | Var of var

(** Finds the location of the variable in the stack,
    returns the stack with the variable removed and the
    former location of the varible if it is found,
    where locations are 1-indexed *)
let find_var_loc (var: var) (s: stack_repr list) :
  (int option * stack_repr list) =
  let indexed = List.mapi (fun i x -> (i + 1,x)) s in
  List.fold_left (fun (ind, new_st) (i,x) ->
                  match ind, x with
                  | None, Var w when w = var -> (Some i, new_st)
                  | _ -> (ind, new_st@[x])) (None, []) indexed


(** Compiles an expression to stack machine instructions while
    maintaining a stack representation to keep track of variable
    locations. This function is explained in great detail in
    documentation.pdf *)
let rec to_stack (ops: operation list) (stack: stack_repr list) :
  (exp -> operation list * stack_repr list) =
  function
  | Int i -> (ops@[Push i], Const::stack)
  | Plus (e1, e2) ->
     let (ops1, st1) = to_stack ops stack e1 in
     let (ops2, st2) = to_stack ops1 st1 e2 in
     begin
       match st2 with
       | _::_::t -> (ops2@[Add], Const::t)
       | _ -> failwith "not enough vals on stack"
     end
  | Let (v, e1, e2) -> let (ops1, st1) = to_stack ops stack e1 in
                       let new_st = match st1 with
                         | _::t -> (Var v)::t
                         | _ -> failwith "not enough vals in stack" in
                       to_stack ops1 new_st e2
  | Var v ->
     let (loc, new_st) = find_var_loc v stack in
     begin
       match loc with
       | None -> raise (UnboundVariable v)
       | Some loc -> (ops@[Roll loc], (Var v)::new_st)
     end
  | App (e1, e2) -> let (ops1, st1) = to_stack ops stack e1 in
                    let (ops2, st2) = to_stack ops1 st1 e2 in
                    begin
                    match st2 with
                    | _::_::t -> (ops2@[Apply], Const::t)
                    | _ -> failwith "not enough vals on the stack"
                    end
  | Lam (v, e) ->
     let (new_st, free_stack, _, roll_ops) =
       place_into_scope e stack [] [Var v] [] in
     let proc_ops = (fst (to_stack [] (Var v::free_stack) e)) in
     (ops@roll_ops@[Form_Closure (proc_ops, List.length free_stack)],
      Const::new_st)

(** Helper function to to_stack which prepares the stack for a
    Form_Closure instruction by moving free variables referenced
    in the body of the function to the bottom of the stack.
    Explained in further detail in documentation.pdf *)
and place_into_scope (e: exp) (orig_stack: stack_repr list)
                       (free_stack: stack_repr list)
                       (local_stack: stack_repr list)
                       (ops: operation list) =
  let orig_args = (orig_stack, free_stack, local_stack, ops) in
  let is_in_stack (var: var) (s: stack_repr list) =
    List.exists (function
                  | Var x -> x = var
                  | Const -> failwith "should not be there") s in
  match e with
  | Var v -> if is_in_stack v local_stack || is_in_stack v free_stack
             then orig_args else
               let (loc, new_st) = find_var_loc v orig_stack in
               begin
                 match loc with
                 | None -> raise (UnboundVariable v)
                 | Some loc -> (new_st, (Var v)::free_stack, local_stack,
                                ops@[Roll loc])
               end
  | Int _ -> orig_args
  | App (e1, e2)
  | Plus (e1, e2) ->
     let (os, fs, ls, ops) =
       place_into_scope e1 orig_stack free_stack local_stack ops in
     place_into_scope e2 os fs ls ops
  | Let (v, e1, e2) ->
     let (os, fs, ls, ops) =
       place_into_scope e1 orig_stack
                        free_stack local_stack ops in
     place_into_scope e2 os fs ((Var v)::ls) ops
  | Lam (v, e) ->
     place_into_scope e orig_stack free_stack ((Var v)::local_stack) ops


(** Returns the list of stack machine instructions given an expression *)
let compile s = fst (to_stack [] [] s)
