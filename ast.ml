open Util
type var = string
type fun_id = int

exception UnboundVariable of var
exception IllformedExpression
exception IllformedDefuncExp

type binop = Plus | Minus | Divide | Multiply

type exp =
    Var   of var
  | App   of exp * exp
  | Int   of int
  | Lam   of var * exp
  | Let   of var * exp * exp
  | Binop of binop * exp * exp

type value = VInt of int | VClosure of env * var * exp
and env = (var * value) list

let empty = []

let lookup g x = 
    try List.assoc x g with Not_found -> raise @@ UnboundVariable x

let extend g x v = 
    (x, v)::g

(* get free variables of an expression *)
let fv (e : exp) : var HashSet.t =
    let h = HashSet.make() in
    let rec fv (bv : var list) (e : exp) : unit =
        match e with
        | Int i -> ()
        | Var x -> if List.mem x bv then () else HashSet.add h x
        | Lam (x, e) -> fv (x::bv) e
        | Let (x, e1, e2) -> fv bv e1; fv (x :: bv) e2
        | App(e1, e2) | Binop (_, e1, e2) -> fv bv e1; fv bv e2 in
        fv [] e; h

(*(* get free variables of an expression *)
let fv_defunc (e : defunc_exp) : var HashSet.t =
    let h = HashSet.make() in
    let rec fv (bv : var list) (e : defunc_exp) : unit =
        match e with
        | Int i -> ()
        | Var x -> if List.mem x bv then () else HashSet.add h x
        | Lam (x, e) -> fv (x::bv) e
        | Let (x, e1, e2) -> fv bv e1; fv (x :: bv) e2
        | App(e1, e2) | Binop (_, e1, e2) -> fv bv e1; fv bv e2 in
        fv [] e; h
*)
type defunc_exp = 
    | Var     of var
    | Int     of int
    | App     of defunc_exp * defunc_exp * int
    | Lam     of fun_id * var list
    | Binop   of binop * defunc_exp * defunc_exp * int

type defunc_value = VInt of int | VClosure of defunc_env * var * defunc_exp
and defunc_env    = (var * defunc_value) list

(* each stored function has form (free variable list, arg, closure body)).
 * Stored functions have unique fun_ids. *)
type stored_fun = var * defunc_exp * int
type closure_directory = (fun_id * stored_fun) list
