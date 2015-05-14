type operation = Push of int | Add | Roll of int | Apply
                 | Form_Closure of operation list * int

type stack = Integer of int | Closure of operation list * stack list

let rec eval_stack (s: stack list) (ops:operation list) =
  let eval_op (stack: stack list) (op: operation) =
    match (stack, op) with
    | (_, Push i) -> (Integer i)::stack
    | (Integer i1::Integer i2::t, Add) -> (Integer (i1+i2))::t
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
  if pos <= i then (ls@[v], ns, pos+1) else (ls, ns@[v], pos+1)) ([], [], 1) s in
    (Closure (ops, local_stack))::new_st
    | (argument::(Closure (opers, st))::t, Apply) ->
       (eval_stack (argument::st) opers) @ t
    | (_, Apply) -> failwith "invalid apply" in
  List.fold_left eval_op s ops