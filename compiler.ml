type stack_value = Const | Var of var

let rec to_stack (ops: ops list) (stack: stack_value list) =
  function
  | Int i -> ((Push i)::ops, Const::stack)
  | Plus (e1, e2) ->
     let (ops1, st1) = to_stack ops stack e1 in
     let (ops2, st2) = to_stack ops1 st1 e2 in
     begin
       match st2 with
       | _::_::t -> (Add::ops2, Const::t)
       | _ -> failwith "not enough vals on stack"
     end
  | Let (v, e1, e2) -> let (ops1, st1) = to_stack ops stack e1 in
                       let new_st = match st1 with
                         | Const::t -> (Var v)::t
                         | _ -> failwith "what to do in var case" in
                       to_stack ops1 new_st e2
  | Var v ->
     let indexed = List.mapi (fun i x -> (i,x)) stack in
     let (loc, new_st) =
       List.fold_left (fun (ind, new_st) (i,x) ->
                       match ind, x with
                       | None, Var w when w = v -> (Some i, new_st)
                       | _ -> (ind, new_st@[x])) (None, []) indexed in
     begin
       match loc with
       | None -> failwith "no such variable"
       | Some loc -> ((Roll (loc + 1))::ops, (Var v)::new_st)
     end
  | _ -> failwith "not done yet"

let compile s =
  let (ops, _) = to_stack [] [] s in
  List.rev ops
