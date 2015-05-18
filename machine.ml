open Ast

type operation = Push of int | Add | Roll of int | Apply
                 | Form_Closure of operation list * int

type stack = Int of int | Closure of operation list * stack list

let rec eval_stack (s: stack list) (ops:operation list) : stack list =
  let eval_op (stack: stack list) ((op_num, op): int * operation) =
    match (stack, op) with
    | (_, Push i) -> (Int i)::stack
    | (Int i1::Int i2::t, Add) -> (Int (i1+i2))::t
    | (_, Add) -> failwith "invalid add"
    | (_, Roll i) ->
       let (_, value, new_st) =
         List.fold_left (fun (count, value, st) x ->
                         if count = i
                         then (count + 1, Some x, st)
                         else (count + 1, value, st@[x]))
                        (1, None, []) stack in
       begin
         match value with
         | None -> failwith "invalid roll"
         | Some num -> num::new_st
       end
    | (_, Form_Closure (ops, i)) ->
       let (local_stack, new_st, _) =
         List.fold_left (fun (ls, ns, pos) v ->
                         if pos <= i then (ls@[v], ns, pos+1)
                         else (ls, ns@[v], pos+1)) ([], [], 1) stack in
       if (List.length local_stack) <> i
       then failwith "invalid form closure"
       else (Closure (ops, local_stack))::new_st
    | (argument::(Closure (opers, st))::t, Apply) ->
       (eval_stack (argument::st) opers) @ t
    | (_, Apply) -> failwith "invalid apply" in
  List.fold_left eval_op s (List.mapi (fun i x -> (i+1, x)) ops)
