open Util
open State
open Machine

module Eval_Writer = struct
    include Writer_Monad (struct
        type t = record_tape
        let empty = ([],[])
        let concat (h_t, h_p) (h_t', h_p') = 
            (h_t' @ h_t, h_p' @ h_p)
    end)
    let record_op op = ((), ([], [op]))
    let record_history i = ((), ([i], []))
end

let eval_binop_op op s = 
    let open Eval_Writer in
    let f = match op with
    | Add -> ( + )
    | Subt -> (- )
    | Mult -> ( * )
    | Div -> ( / ) in
    match s with
    | Stack_Int i1::Stack_Int i2::t_stack ->
            record_history i2 >>
            return @@ Stack_Int (f i1 i2)::t_stack
    | _ -> raise IllformedProgram

let eval_func_op op ((p, s, s_f) : machine) : machine * record_tape =
    let open Eval_Writer in
    match op, s with
    | Save_Function (id, k), _ ->
            begin 
                match split_list p k, List.mem_assoc id s_f with
                | Some (func_p, p'), false ->
                        return (p', s, (id, func_p)::s_f)
                | _ -> raise IllformedProgram
      end
    | Form_Closure (id), Tuple tup::s' ->
            let func_p = List.assoc id s_f in
            return (p, Closure(func_p, tup)::s', s_f)
    | Apply, Closure(func_p, tup)::arg::s' ->
            record_history @@ List.length func_p >>
            return (func_p @ p, tup @ (arg::s'), s_f)
    | _ -> raise IllformedProgram

let eval_pair_op op s =
    match op, s with
    | Construct_Tuple k, _ -> 
            begin
                match split_list s k with
                | Some (tuple, s') -> 
                        Tuple (tuple)::s'
                | _ -> raise IllformedProgram
            end
    | Deconstruct_Tuple k, Tuple tuple::s' ->
            tuple @ s'
    | Roll k, _ ->
            begin
                match split_list s (k-1) with 
                | Some (l1, elem::l2) -> elem::(l1@l2)
                | _ -> raise IllformedProgram
            end
    | Unroll k, _ -> 
            begin
                match split_list s k with
                | Some (elem::l1, l2) -> l1 @ [elem] @ l2
                | _ -> raise IllformedProgram
            end
    | _ -> raise IllformedProgram

let eval_op op m = 
    let open Eval_Writer in
    let (p, s, s_f) = m in
    match op with
    | Binop_Op b -> 
            eval_binop_op b s >>= fun s' ->
            return (p, s', s_f)
    | Func_Op f -> eval_func_op f m
    | Pair_Op a -> 
            let s' = eval_pair_op a s in
            return (p, s', s_f)
    | Push n -> return (p, Stack_Int n::s, s_f)
    | Skip f -> 
            let () = f () in return m

let rec eval_machine m =
    let open Eval_Writer in
    let (p, s, s_f) = m in
    match p with
    | [] -> return m
    | op::p' -> 
            record_op op >> 
            eval_op op (p', s, s_f) >>= fun m' ->
            eval_machine m'

module Reverse_State = struct
    include State_Monad (struct type t = history_tape end)
    let put s' = fun s -> ((), s')
    let pop = function h::t -> (h,t) | [] -> raise IllformedHistory
end

let reverse_binop_op op s : stack Reverse_State.t =
    let open Reverse_State in
    let inv_f = match op with
    | Add -> ( - )
    | Subt -> ( + )
    | Div -> ( * )
    | Mult -> ( / ) in
    match s with
    | Stack_Int result::s' ->
            pop >>= fun i2 ->
            return (Stack_Int (inv_f result i2)::Stack_Int i2::s')
    | _ -> raise IllformedHistory

let reverse_func_op op (p, s, s_f) : machine Reverse_State.t =
    let open Reverse_State in
    match op, s with
    | Save_Function (id, k), _-> 
            if not @@ List.mem_assoc id s_f
            then raise IllformedHistory
            else 
                let p' = List.assoc id s_f in
                let s_f' = List.remove_assoc id s_f in
                return (p' @ p, s, s_f')
    | Form_Closure (id), Closure (p_func, tup)::s' ->
            return (p, Tuple tup::s', s_f)
    | Apply, Tuple tup::arg::s' ->
            pop >>= fun k ->
            begin
                match split_list p k with
                | Some (func_p, p') -> 
                        return (p', Closure(func_p, tup)::arg::s', s_f)
                | _ -> raise IllformedHistory
            end
    | _ -> raise IllformedHistory

let reverse_pair_op op s =
    let pair = 
        match op with
        | Construct_Tuple k -> Deconstruct_Tuple k
        | Deconstruct_Tuple k -> Construct_Tuple k
        | Roll k -> Unroll k
        | Unroll k -> Roll k in
    try
        eval_pair_op pair s
    with IllformedProgram -> raise IllformedHistory

let reverse_op op m : machine Reverse_State.t =  
    let open Reverse_State in
    let (p, s, s_f) = m in
    match op, s with
    | Binop_Op b, _ -> 
            reverse_binop_op b s >>= fun s' -> 
            return (p, s', s_f)
    | Func_Op f, _ -> reverse_func_op f m
    | Pair_Op t, _ -> 
            let s' = reverse_pair_op t s in
            return (p, s', s_f)
    | Push n, Stack_Int i::s' when i = n -> return (p, s', s_f)
    | Skip _, _ -> return m
    | _ -> raise IllformedHistory

let reverse_history (m, (h_t, h_p)) : machine =
    let open Reverse_State in
    let final_state =
        List.fold_left (fun m op ->
            m >>= fun m ->
            reverse_op op m >>= fun ((p', s', s_f'):machine) ->
            return (op::p', s', s_f')) (return m) h_p in
    match final_state h_t with
    | m, [] -> m
    | _ -> raise IllformedHistory
