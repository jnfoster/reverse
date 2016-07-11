open Util
open Ast
open State
open Machine
exception IllformedExpression

let rec eval (e:exp) (s: state) : value =
  match e with
    | Var x -> lookup s x
    | App(e1,e2) ->
      begin
        match eval e1 s with
          | VClosure (Lam (x::t, e), c) ->
                  let v = eval e2 s in
                  eval (Lam (t, e)) (update c x v)
          | _ -> raise IllformedExpression
      end
    | Lam([], e) -> eval e s
    | Lam _ -> VClosure (e, s)
    | Let(x,e1,e2) ->
            eval (App(Lam([x],e2), e1)) s
    | Int n -> VInt n
    | Binop (op, e1,e2) ->
      begin
        match eval e1 s, eval e2 s with
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
  | _, Skip f::t_ops -> eval_stack (t_ops, stack, history_tape, Skip f::history_prog)
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
  | _, (Add|Mult|Div|Subt)::t_ops -> 
          failwith "invalid binop"
  | _, (Roll i as roll)::t_ops ->
      let s' = roll_list stack i in
      eval_stack (t_ops, s', history_tape, roll:: history_prog)
  | _, (Unroll i as unroll)::t_ops ->
      let s' = unroll_list stack i in
      eval_stack (t_ops, s', history_tape, unroll:: history_prog)
  | _, (Form_Closure (num_vals, num_ops) as fc)::t_ops ->
      begin 
        match split_list t_ops num_ops with
        | Some (local_ops, t_ops) ->
            let s' = Closure (num_vals, local_ops)::stack in
            eval_stack (t_ops, s', history_tape, fc::history_prog)
        | _ -> failwith "invalid form closure"
      end
  | (Closure (n1, local_ops))::t_stack, (MultiApply n2 as ma)::t_ops when n1 = n2 ->
      let ops' = local_ops @ t_ops in
      eval_stack (ops', t_stack, 
      (List.length local_ops)::history_tape, ma::history_prog)
  | _, MultiApply _::_ -> 
          let () = Pprint.print_stack stack in
          failwith "invalid apply"
  | _::[], [] -> state 
  | _, [] -> failwith "unused variable on stack"



  let reverse_history (program, stack, history_tape, history_prog) = 
    (* Makes the top element of the stack the i-th element,
     * where the stack indexing starts at 1 *)
    let reverse (p, s, h_t) op  =
            
      match (op, s, h_t) with 
      | Skip f, _, _ -> (Skip f::p, s, h_t) 
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
      | Form_Closure(n1, num_ops) as fc, Closure(n2, local_ops)::s, _ when n1 = n2 ->
          (fc::local_ops @ p, s, h_t)
      | Form_Closure _, _, _ ->
          failwith "invalid un-form_closure"
      | MultiApply n1 as ma, _, num_ops::h_t  when List.length p >= num_ops && List.length s >= n1->
          begin 
            match split_list p num_ops with
            | Some (local_p, p) -> 
                let cl = Closure(n1, local_p) in
                let s' = cl::s in
                (ma::p, s', h_t) 
            | _ -> failwith "invalid un-apply"
      end
      | MultiApply _, _, _ -> 
              let () = Format.printf "hello!-----\n" in
              let () = Pprint.print_instrs p in
              let () = Pprint.print_stack stack in
              failwith "invalid un-apply" in
              let (restored_prog, restored_stack, restored_history) = 
      List.fold_left reverse (program, stack, history_tape) history_prog in
    (restored_prog, restored_stack, restored_history, [])

