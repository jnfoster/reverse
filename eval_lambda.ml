open Util
open Ast

(** Evaluates an expression in the source language
 * without translating to machine instructions. *)
let eval_lambda (e:exp) : value =
    let rec eval (g:env) (e:exp) : value = 
        match e with
        | Var x -> lookup g x
        | App (e1, e2) ->    
                begin
                    match eval g e1 with
                    | VClosure (g', x, body) ->
                            let v2 = eval g e2 in
                            let g2 = extend g' x v2 in
                            eval g2 body
                    | _ -> raise IllformedExpression
                end
        | Lam (x, body) -> VClosure (g, x, body)
        | Let(x,e1,e2) ->
                eval g (App (Lam (x,e2), e1))
        | Int n -> VInt n
        | Binop (op, e1, e2) ->
                let fun_binop = function
                    | Plus   -> ( + ) | Minus    -> ( - )
                    | Divide -> ( / ) | Multiply -> ( * ) in
                begin 
                    match eval g e1, eval g e2 with
                    | VInt n1, VInt n2 -> VInt (fun_binop op n1 n2)
                    | _ -> raise IllformedExpression
                end in
        eval empty e

module Defunc_Writer = struct
    include Writer_Monad (struct
        type t = closure_directory
        let empty = []
        let concat d d' = d @ d'
      end)
      let add_func id stored_fun = 
          ((), [(id, stored_fun)])
    end

(* Assume e is linear program *)
let defunctionalize (e:exp) : defunc_exp * closure_directory = 
    let open Defunc_Writer in
    let id_count = ref 1 in 
    let new_id () = 
        let id = !id_count in
        id_count := !id_count + 1; id in
    
    let rec defunc (e: exp) : (defunc_exp  * var list) * closure_directory = 
        match e with
        | Var x -> return (Var x, [x])
        | Int n -> return (Int n, [])
        | Lam (arg, e) -> 
                let fun_id = new_id () in
                defunc e >>= fun (body, fv_e) ->
                let (pos_arg, _) = 
                    let indexed = 
                        List.mapi (fun i var -> (i + 1, var)) fv_e in
                    List.find (fun (pos, var) -> var = arg) indexed in
                let fv_e' = 
                    List.filter ((=) arg) fv_e in
                add_func fun_id (arg, body, pos_arg) >>
                return (Lam (fun_id, fv_e'), [])
        | Let (x, e1, e2) ->
                defunc @@ App (Lam (x, e2), e1)
        | App (e1, e2) ->
                defunc e1 >>= fun (e1', fv_e1) ->
                defunc e2 >>= fun (e2', fv_e2) ->
                return @@ (App (e1', e2', List.length fv_e1), fv_e1 @ fv_e2)
        | Binop (b, e1, e2) ->
                defunc e1 >>= fun (e1', fv_e1) ->
                defunc e2 >>= fun (e2', fv_e2) ->
                return (Binop (b, e1', e2', List.length fv_e1), fv_e1 @ fv_e2) in
        let ((exp, _), dir) =  defunc e in
        (exp, dir)

let eval_defunc defunc_e clos_dir : defunc_value = 
    let rec eval e env : defunc_value =
        match e with
        | Var x -> lookup env x
        | Int n -> VInt n
        | App (e1, e2, _) -> 
                begin
                    match eval e1 env with
                    | VClosure (env', x, body) ->
                            let v2 = eval e2 env in
                            let env'' = extend env' x v2 in
                            eval body env''
                    | _ -> raise IllformedDefuncExp
                end
        | Lam (fun_id, fv) ->
                let env' = 
                    let fv_vals = List.map (fun v -> lookup env v) fv in
                    List.combine fv fv_vals in
                let (x, body) = 
                    try List.assoc fun_id clos_dir
                    with Not_found -> raise IllformedDefuncExp in
                VClosure (env', x, body)
        | Binop (b, e1, e2, _) ->
                let fun_binop = function
                    | Plus   -> ( + ) | Minus    -> ( - )
                    | Divide -> ( / ) | Multiply -> ( * ) in
                begin 
                    match eval e1 env, eval e2 env with
                    | VInt n1, VInt n2 -> VInt (fun_binop b n1 n2)
                    | _ -> raise IllformedDefuncExp
                end in
    eval defunc_e []



