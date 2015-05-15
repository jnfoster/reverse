open Ast
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
    | Plus(e1,e2) ->
      begin
        match eval g e1, eval g e2 with
          | VInt n1, VInt n2 ->
            VInt(n1 + n2)
          | _ ->
            raise IllformedExpression
      end
