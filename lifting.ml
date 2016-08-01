open Util
open Ast
open State

let rec is_target_exp (e : exp) : bool =
  match e with 
    | Var _ -> true
    | Lam  _ -> false 
    | Let _ -> false
    | App (e1, e2) | Binop (_, e1, e2) -> is_target_exp e1 && is_target_exp e2
    | Int _ -> true

let rec is_target_fun (e : exp) : bool = 
  match e with
    | Lam(_,e1) -> is_target_exp e1
    | _ -> false

let rec is_target_prog (e : exp) : bool = 
  match e with 
    | Let(_,e1,p2) -> is_target_fun e1 && is_target_prog p2
    | _ -> is_target_exp e

let convert (e : exp) (s : state) : exp * state =
  let fresh = Fresh.make (Ast.allv e) in
  let rec convert (e : exp) (s: state) : exp * state = 
      let convert2 e1 e2 s = 
          let (e1', s') = convert e1 s in
          let (e2', s'') = convert e2 s' in
          (e1', e2', s'') in
      match e with
      | Var _ -> (e,s)
      | Lam (x, Lam(y, e)) ->
              convert (Lam (x @ y, e)) s
      | Lam (xs,e) ->
              let (e', s') = convert e s in
              let fv = HashSet.values (Ast.fv (Lam (xs, e'))) in
              let h = Fresh.next fresh in
              let s'' = State.update s' h (VClosure (Lam (fv @ xs, e'), State.make ())) in
              let f = List.fold_left (fun e x -> App (e, x)) 
              (Var h) (List.map (fun x -> Var x) fv) in
              (f, s'')
      | Let(x, e1, e2) -> convert (App (Lam ([x], e2), e1)) s
      | App(e1,e2) -> 
              let (e1', e2', s') = convert2 e1 e2 s in
              (App (e1', e2'), s')
      | Int _ -> (e,s)
      | Binop (b, e1, e2) -> 
              let (e1', e2', s') = convert2 e1 e2 s in
              (Binop (b, e1', e2'), s') in
      convert e s

let lift (e : exp) : exp = 
    let rec lift (e: exp) (s: (var * value) list) : exp =
        match s with 
        | [] -> e
        | (h, VClosure (g, c)):: t -> 
                begin match State.bindings c with
                | [] -> lift (Let (h, g, e)) t
                | _ -> failwith "Error while lifting"
                end
        | _ -> failwith "Error while lifting" in
        let (e, s) = convert e (State.make ()) in
        lift e (State.bindings s)

