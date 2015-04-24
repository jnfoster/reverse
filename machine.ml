type ops = Push of int | Add | Roll of int

let eval_stack ops =
  let eval_op stack op =
    match (stack, op) with
    | (_, Push i) -> i::stack
    | (v1::v2::t, Add) -> (v1+v2)::t
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
       end in
  List.fold_left eval_op [] ops
