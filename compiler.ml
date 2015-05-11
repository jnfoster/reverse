type stack_repr = Const | Var of var

let rec to_stack (ops: operation list) (stack: stack_repr list) =
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
                         | _::t -> (Var v)::t
                         | _ -> failwith "not enough vals in stack" in
                       to_stack ops1 new_st e2
  | Var v ->
     let indexed = List.mapi (fun i x -> (i + 1,x)) stack in
     let (loc, new_st) =
       List.fold_left (fun (ind, new_st) (i,x) ->
                       match ind, x with
                       | None, Var w when w = v -> (Some i, new_st)
                       | _ -> (ind, new_st@[x])) (None, []) indexed in
     begin
       match loc with
       | None -> failwith "no such variable"
       | Some loc -> ((Roll loc)::ops, (Var v)::new_st)
     end
  | App (e1, e2) -> let (ops1, st1) = to_stack ops stack e1 in
                    let (ops2, st2) = to_stack ops1 st1 e2 in
                    (* top of stack contains e2, under is it a function,
                     where do we check that e1 is a function?*)
                    begin
                    match st2 with
                    | _::_::t -> (Apply::ops2, Const::t)
                    | _ -> failwith "not enough vals on the stack"
                    end
  | Lam (v, e) ->
     let proc_ops = List.rev (fst (to_stack [] (Var v::stack) e)) in
     ((Form_Closure proc_ops)::ops, Const::stack)

let compile s =
  let (ops, _) = to_stack [] [] s in
  List.rev ops
