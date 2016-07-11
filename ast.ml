open Util

type var = string

type binop = Plus | Minus | Divide | Multiply

type exp =
    Var of var
  | App of exp * exp
  | Lam of var list * exp
  | Let of var * exp * exp
  | Int of int
  | Binop of binop * exp * exp

exception UnboundVariable of var

(* get free variables of an expression *)
let fv (e : exp) : var HashSet.t =
  let h = HashSet.make() in
  let rec fv (bv : var list) (e : exp) : unit =
    match e with
    | Int i -> ()
    | Var x -> if List.mem x bv then () else HashSet.add h x
    | Lam (x, e) -> fv (x @ bv) e
    | Let (x, e1, e2) -> fv bv e1; fv (x :: bv) e2
    | App(e1, e2) | Binop (_, e1, e2) -> fv bv e1; fv bv e2 in
  fv [] e; h
  
(* get all variables of an expression *)
let allv (e : exp) : var HashSet.t =
  let h = HashSet.make() in
  let rec allv (e : exp) : unit =
    match e with
    | Int _ -> ()
    | Var x -> HashSet.add h x
    | Lam (x, e) -> List.iter (HashSet.add h) x; allv e
    | Let (x, e1, e2) -> HashSet.add h x; allv e1; allv e2
    | App(e1, e2) | Binop (_, e1, e2) -> allv e1; allv e2 in
  allv e; h

(* substitute v for x in e, avoiding capture *)
let subst (v : exp) (x : var) (e : exp) : exp = 
    let fresh = Fresh.make (allv e) in
    let fvv = fv v in
    let rec subst (v : exp) (x : var) (e : exp) : exp =
        match e with
        | Int _ as i -> i
        | Var y -> if x = y then v else Var y
        | Lam (y :: t, e1) ->
                if x = y then e else
                    let e0 = Lam (t, e1) in
                    let (z, e0) =
                        if HashSet.mem fvv y
                        then let z = Fresh.next fresh in
                        (z, subst (Var z) y e0)
                else (y, e0) in
                    begin 
                        match subst v x e0 with
                        | Lam (t, e1') -> Lam (z :: t, e1')
                        | _ -> failwith "System error"
                    end
        | Lam ([], e1) -> Lam ([], subst v x e1)
        | App (e1, e2) -> App (subst v x e1, subst v x e2)
        | Let (x, e1, e2) ->
                begin 
                    match subst v x (Lam ([x], e2)), subst v x (Lam ([], e1)) with
                    | Lam([x], e2'), Lam ([], e1') -> Let (x, e1', e2')
                    | _ -> failwith "System error"
                end
        | Binop (b, n1, n2) -> Binop (b, subst v x n1, subst v x n2) in
  subst v x e
