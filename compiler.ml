open Util
open Ast
open Machine
open Pprint

    let rec simplify_roll p =
        let rec simple p = match p with
        | [] -> []
        | Unroll i1::Roll i2::t
        | Roll i1 :: Unroll i2::t  when i1 = i2 -> simple t
        | Unroll 1::t | Roll 1::t -> simple t
        | Form_Closure (_, num_ops) as fc::t-> 
                begin 
                    match split_list t num_ops with
                    | None -> failwith "not expected"
                    | Some (cl, t_ops) -> fc::cl @ (simple t_ops)
                end
        | x::t -> x::(simple t) in
        match simple p with
        | [] -> [Roll 1]
        | l when List.length l < List.length p -> simplify_roll l
        | l -> l
    
let rec compile (e: exp) var_stack =

    let update_roll instrs r u = 
        List.fold_left (fun acc x ->
            match x with Roll i -> acc @ [Roll (i + r); Unroll (u + 1)]
            | _ -> failwith "not expected") [] instrs in

    let place_into_scope e s = 
        let find_var_loc var s = 
            let indexed = List.mapi (fun i x -> (i + 1, x)) s in
            List.fold_left (fun (ind, new_st) (i, x) ->
                match ind, x with
                | None, Stack_Var w when w = var -> (Some i, new_st)
                | _ -> (ind, new_st @ [x])) (None, []) indexed in

        (* todo get the free variables then get them onto the stack *)
        let free = HashSet.values (Ast.fv e) in
        List.fold_left (fun (st, fvs, roll_ops) x ->
            match find_var_loc x st with
            | None, _ -> failwith "not expected"
            | Some ind, st' -> 
                    (st', (Stack_Var x)::fvs, roll_ops @ [Roll ind])) 
        (s, [], []) free in

    match e with
    | Int i -> 

            let () = assert (var_stack = []) in
            ([Push i], 1, [])

    | Var v ->

            let () = assert (var_stack = [Stack_Var v]) in
            ([], 0, [])

    | Binop (op, e1, e2) ->
            
            let instr = match op with
                | Plus -> Add
                | Minus -> Subt
                | Divide -> Div
                | Multiply -> Mult in

            let (st', fv_e2, roll_e2) = place_into_scope e2 var_stack in
            let (grow_e2, grow_s_e2, shrink_e2) = compile e2 fv_e2 in

            let () = assert (HashSet.size (Ast.fv e1) =
            List.length var_stack - List.length fv_e2) in

            let (grow_e1, grow_s_e1, shrink_e1) = compile e1 st' in

            let roll_e2' = update_roll roll_e2 (grow_s_e1 + grow_s_e2) grow_s_e2 in

            (* Place the result of evaluating e2 at bottom of stack *)
            let unroll_result = [Unroll (List.length st' + grow_s_e1 + 1)] in

            (* e1 -> v1 and e2 -> v2, and v1 should be at the top of the
             * stack with v2 below (only elements on stack) *)
            (grow_e1 @ grow_e2, grow_s_e1 + grow_s_e2,
            roll_e2' @ shrink_e2 @ unroll_result @ shrink_e1 @ [instr])

    | Let (x, e1, e2) ->
            
            let (st', fv_e1, roll_e1) = place_into_scope e1 var_stack in
            let (grow_e1, grow_s_e1, shrink_e1) = compile e1 fv_e1 in

            let () = assert (HashSet.mem (Ast.fv e2) x) in

            let () = assert (HashSet.size (Ast.fv e2) - 1 =
            List.length var_stack - List.length fv_e1) in

            let (grow_e2, grow_s_e2, shrink_e2) = compile e2 ((Stack_Var x)::st') in

            let roll_e1' = update_roll roll_e1 (grow_s_e2 + grow_s_e1) grow_s_e1 in

            (* e1 -> x is placed top of the fvs used in e2 *)
            let unroll_var = [Unroll (grow_s_e2 + 1)] in

            (grow_e2 @ grow_e1, grow_s_e2 + grow_s_e1,
            roll_e1' @ shrink_e1 @ unroll_var @ shrink_e2)
    
    | Lam (l, e) ->

            let l' = List.map (fun x -> Stack_Var x) l in
            
            let (grow_e, _, shrink_e) = compile e l' in

            let simple_e = simplify_roll (grow_e @ shrink_e) in 
            
            ([Form_Closure (List.length l, List.length simple_e)] @ simple_e, 1, [])

    | App (e1, e2) ->

            let (f, args) = 
                let rec fold_app (f, acc) e = match e with
                | App (e1, e2) -> 
                        fold_app (e1, e2::acc) e1
                | _ -> (f, acc) in
                fold_app (e1, [e2]) e1 in

            let (st', ops) = 
                List.fold_left (fun (st, ops) e ->
                    let (st', fv_e, roll_e) = place_into_scope e st in
                    (st', ops @ [(compile e fv_e, roll_e)])) (var_stack, []) (f::args) in

            (* should consume all vars on stack *)
            let () = assert (List.length st' = 0) in

            let total_growth = 
                List.fold_left (fun sum ((_, g, _), _) -> sum + g) 0 ops in
            let total_vars =
                List.length var_stack in

            let (grow_ops, shrink_ops, g, n_fvs, n_vals) =

                let combine_ops (grow_ops, shrink_ops, growth, num_fvs, num_vals) 
                ((grow_e, grow_s_e, shrink_e), roll_e) = 

                    let roll_e' = update_roll roll_e growth grow_s_e in
                    let growth' = growth - grow_s_e in
                    let num_fvs' = num_fvs - (List.length roll_e) in
                    let unroll_v = [Unroll (growth' + num_fvs' + num_vals + 1)] in

                    (grow_e @ grow_ops, shrink_ops @ roll_e' @ shrink_e @ unroll_v, 
                    growth', num_fvs', num_vals + 1) in
                
                List.fold_left combine_ops ([], [], total_growth, total_vars, 0) ops in
            
            let () = assert (g = 0) in 
            let () = assert (n_fvs = 0) in
            let () = assert (n_vals = List.length args + 1) in
            (grow_ops, total_growth, shrink_ops @ [MultiApply (List.length args)])
                    

(** Returns the list of stack machine instructions given an expression *)
let translate (init_repr:stack_repr) (e: exp) : program = 
    let () = assert (Lifting.is_target_prog e) in
    let (grow_ops, _, shrink_ops) = compile e init_repr in
    simplify_roll (grow_ops @ shrink_ops)
