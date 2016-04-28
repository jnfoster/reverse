open Common
open Ast
open Machine
open Pprint

(** Helper function to to_stack which prepares the stack for a
    Form_Closure instruction by moving free variables referenced
    in the body of the function to the bottom of the stack.
    Explained in further detail in documentation.pdf *)

let place_into_scope e stack bound_vars = 
 
    let rec place (e: exp) (stack, free_vars, bound_vars, ops)  =
        let orig_args = (stack, free_vars, bound_vars, ops) in
    (** Finds the location of the variable in the stack,
     * returns the stack with the variable removed and the
     * former location of the varible if it is found,
     * where locations are 1-indexed *)
    
    let find_var_loc var s =
        
        let indexed = List.mapi (fun i x -> (i + 1,x)) s in
        List.fold_left (fun (ind, new_st) (i,x) ->
            match ind, x with
            | None, Stack_Var w when w = var -> (Some i, new_st)
            | _ -> (ind, new_st@[x])) (None, []) indexed in
    let is_in_stack var s =
        List.exists (function
            | Stack_Var x -> x = var
            | Const -> false) s in
   
    match e with
    | Var v -> 
            if is_in_stack v free_vars || is_in_stack v bound_vars
            then orig_args else
                let (loc, new_st) = find_var_loc v stack in
                begin
                    match loc with
                    | None -> raise (UnboundVariable v)
                    | Some loc -> (new_st, (Stack_Var v)::free_vars, bound_vars,
                    ops@[Roll loc])
                end
    | Int _ -> orig_args
    | App (e1, e2) | Binop (_, e1, e2) ->
            place e2 @@ place e1 @@ orig_args
    | Let (v, e1, e2) ->
            let (s, fv, bv, ops) = place e1 orig_args in
            place e2 (s, fv, ((Stack_Var v)::bv), ops)
    | Lam (v, e) ->
            place e (stack, free_vars, ((Stack_Var v)::bound_vars), ops) in
    place e (stack, [], bound_vars, [])


let rec compile (e: exp) var_stack = 
    
    let update_roll instrs d = 
        List.map (function Roll i -> Roll (i + d) | _ -> failwith "not expected") instrs in
    
    match e with
    | Int i -> 
            let () = assert (var_stack = []) in
            ([Push i], [Const], [])
    | Var v ->
            let () = assert (var_stack = [Stack_Var v]) in
            ([], [], [])
    | Binop (op, e1, e2) ->

            let instr = match op with
                | Plus -> Add
                | Minus -> Subt
                | Divide -> Div
                | Multiply -> Mult in

            let (s', fv_e1, _, roll_e1) = place_into_scope e1 var_stack [] in
            let (grow_e1, grow_s_e1, shrink_e1) = compile e1 fv_e1 in
            let roll_e1' = update_roll roll_e1 (List.length grow_s_e1) in

            let (s'', fv_e2, _, roll_e2) = place_into_scope e2 (grow_s_e1 @ fv_e1 @ s') [] in   
            let (grow_e2, grow_s_e2, shrink_e2) = compile e2 fv_e2 in
            let roll_e2' = update_roll roll_e2 (List.length grow_s_e2) in

            (grow_e1 @ grow_e2, grow_s_e1 @ grow_s_e2, 
            roll_e2' @ shrink_e2 @ [Unroll (List.length (fv_e1 @ grow_s_e1) + 1)] @ roll_e1' @ shrink_e1 @ [instr])

    | Let (x, e1, e2) ->

            let (s', fv_e1, _, roll_e1) = place_into_scope e1 var_stack [] in
            let (grow_e1, grow_s_e1, shrink_e1) = compile e1 fv_e1 in
            let roll_e1' = update_roll roll_e1 (List.length grow_s_e1) in
            
            let (_, fv_e2, _, roll_e2) = place_into_scope e2 s' [Stack_Var x] in
            let (grow_e2, grow_s_e2, shrink_e2) = compile e2 (fv_e2 @ [Stack_Var x]) in
            let roll_e2' = update_roll roll_e2 (List.length grow_s_e2) in
            
            (grow_e2 @ grow_e1, grow_s_e2 @ grow_s_e1, 
            roll_e1' @ shrink_e1 @ [Unroll (List.length (fv_e2 @ grow_s_e2) + 1)] @ roll_e2' @ shrink_e2)
    
    | _ -> failwith "todo"


(** Returns the list of stack machine instructions given an expression *)
let translate (init_repr:stack_repr) (e: exp) : program = 
    let (grow_ops, _, shrink_ops) = compile e init_repr in
    grow_ops @ shrink_ops
