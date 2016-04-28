open Common
open Ast
open Machine
exception IllformedExpression

let rec eval (g:env) (e:exp) : value =
  match e with
    | Var x -> g x
    | App(e1,e2) ->
      begin
        match eval g e1 with
          | VClosure(g',x,body) ->
            let v2 = eval g e2 in
            let g2 = extend g' x v2 in
            eval g2 body
          | _ ->
            raise IllformedExpression
      end
    | Lam(x,body) ->
      VClosure(g,x,body)
    | Let(x,e1,e2) ->
      eval g (App(Lam(x,e2), e1))
    | Int n ->
      VInt n
    | Binop (op, e1,e2) ->
      begin
        match eval g e1, eval g e2 with
          | VInt n1, VInt n2 ->
                  begin match op with 
                  | Plus -> VInt(n1 + n2)
                  | Minus -> VInt (n1 - n2)
                  | Divide -> VInt (n1 / n2)
                  | Multiply -> VInt (n1 * n2)
                  end
          | _ ->
            raise IllformedExpression
      end

(** Evaluates a list of instructions on the stack.
    Effect of each instruction explained in documentation.pdf *)
let rec eval_stack (state: machine_state) : machine_state =
    let (input_prog, stack, history_tape, history_prog) = state in
  match (stack : stack), input_prog with 
  | _, (Push i as push)::t_ops ->  
      let s' = (Stack_Int i)::stack in 
      eval_stack (t_ops, s', history_tape, push::history_prog)
  | Stack_Int i1::Stack_Int i2::t_stack, ((Add | Mult | Div | Subt) as op)::t_ops -> 
          let f = match op with 
          | Add -> ( + )
          | Mult -> ( * )
          | Div -> ( / )
          | Subt -> ( - )
          | _ -> failwith "should not happen" in
      let s' = Stack_Int (f i1 i2)::t_stack in
      eval_stack (t_ops, s', i2::history_tape, op::history_prog)
  | _, (Add|Mult|Div|Subt)::t_ops -> failwith "invalid binop"
  | _, (Roll i as roll)::t_ops ->
      let s' = roll_list stack i in
      eval_stack (t_ops, s', history_tape, roll:: history_prog)
  | _, (Unroll i as unroll)::t_ops ->
      let s' = unroll_list stack i in
      eval_stack (t_ops, s', history_tape, unroll:: history_prog)
  | _, (Form_Closure (num_ops, num_vars) as fc)::t_ops ->
      begin 
        match split_list t_ops num_ops, split_list stack num_vars with
        | Some (local_ops, t_ops), Some (local_stack, t_stack) ->
            let s' = Closure (local_ops, local_stack)::t_stack in
            eval_stack (t_ops, s', history_tape, 
            fc::history_prog)
        | _ -> failwith "invalid form closure"
      end
  | argument::(Closure (local_prog, local_stack))::t_stack, Apply::t_ops ->
      let s' = argument::local_stack@t_stack in
      let ops' = local_prog@t_ops in
      eval_stack (ops', s', 
      (List.length local_prog)::(List.length local_stack)::history_tape, 
      Apply::history_prog)
  | _, Apply::_ -> failwith "invalid apply"
  | _::[], [] -> state 
  | _, [] -> failwith "unused variable on stack"



  let reverse_history (program, stack, history_tape, history_prog) = 
    (* Makes the top element of the stack the i-th element,
     * where the stack indexing starts at 1 *)
    let reverse (p, s, h_t) op  =
      match (op, s, h_t) with 
      | Push i as push, Stack_Int v::s, _ when i = v -> (push::p, s, h_t)
      | Push _, Stack_Int v::s, _ -> failwith "invalid un-push"
      | Push _, _, _ -> failwith "nothing to un-push"
      | Roll i as roll, _, _ -> 
          let s' = unroll_list s i in
          (roll::p, s', h_t)
      | Unroll i as unroll, _, _ ->
              let s' = roll_list s i in
              (unroll::p, s', h_t)
      | ((Add | Mult | Div | Subt) as binop), Stack_Int result::s, n1::h_t ->
              let inv_f = match binop with
              | Add -> ( - )
              | Subt -> ( + )
              | Div -> ( * )
              | Mult -> ( / ) 
              | _ -> failwith "should not happen" in
          (binop::p, Stack_Int (inv_f result n1)::Stack_Int n1::s, h_t)
      | (Add | Mult | Div | Subt), _, _ -> failwith "invalid un-binop"
      | Form_Closure(num_ops, num_vars) as fc, Closure(local_p, local_s)::s, _ ->
          (fc::local_p@p, local_s@s, h_t)
      | Form_Closure _, _, _ ->
          failwith "invalid un-form_closure"
      | Apply, arg::s, num_ops::num_vars::h_t ->
          begin 
            match split_list p num_ops, split_list s num_vars with
            | Some (local_p, p), Some (local_s, s) -> 
                let cl = Closure(local_p, local_s) in
                let s' = arg::cl::s in
                (Apply::p, s', h_t) 
            | _ -> failwith "invalid un-apply"
      end
      | Apply, _, _ -> failwith "invalid un-apply" in
    let (restored_prog, restored_stack, restored_history) = 
      List.fold_left reverse (program, stack, history_tape) history_prog in
    (restored_prog, restored_stack, restored_history, [])

