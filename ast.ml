type var = string

type binop = Plus | Minus | Divide | Multiply

type exp =
    Var of var
  | App of exp * exp
  | Lam of var * exp
  | Let of var * exp * exp
  | Int of int
  | Binop of binop * exp * exp

exception UnboundVariable of var

type value = 
  | VInt of int 
  | VClosure of env * var * exp

and env = var -> value

let empty : env = 
  (fun x -> raise (UnboundVariable x))

let lookup (g:env) (x:var) : value = 
  g x

let extend (g:env) (x:var) (v:value) : env = 
  (fun y -> if x = y then v else g y)     

