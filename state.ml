open Ast

type value = VInt of int | VClosure of exp * state
and state = (var * value) list

let make () : state = []

let merge = (@)

let bindings s : (var * value) list = s

let lookup (g:state) (x:var) : value = 
  try List.assoc x g
  with Not_found -> raise (UnboundVariable x)

let update (g:state) (x:var) (v:value) : state = 
  (x, v)::g  

