open Ast
open Machine

(* Pretty printing helper functions *)
let print_ident x =
  Format.printf "%s" x

let print_int n =
  Format.printf "%d" n

let print_binop p s x y =
  Format.printf "@[<2>(";
  p x;
  Format.printf "@ %s@ " s;
  p y;
  Format.printf ")@]"

let print_lambda p x e =
  Format.printf "@[<2>(lambda %s.@ " x;
  p e;
  Format.printf ")@]"

let print_let p x e1 e2 =
  Format.printf "@[<2>let %s =@ " x;
  p e1;
  Format.printf "@ in@ ";
  p e2;
  Format.printf "@]"

(* Pretty print expression e *)
let print_exp e =
  let rec loop e =
    match e with
      | Var x -> print_ident x
      | App (l,r) -> print_binop loop "" l r
      | Lam(x,e) -> print_lambda loop x e
      | Let(x,e1,e2) -> print_let loop x e1 e2
      | Plus (l,r) -> print_binop loop "+" l r
      | Int n -> print_int n  in
  Format.printf "@[";
  loop e;
  Format.printf "@]"

(* Pretty print value v *)
let print_val v =
  let rec loop v =
    match v with
      | VInt n ->
        Format.printf "%d" n
      | VClosure(_) ->
        Format.printf "<fun>" in
  Format.printf "@[";
  loop v;
  Format.printf "@]"

(* Pretty print program *)
let print_instrs (lst: program) =
  let rec loop inst =
    match inst with
    | Push i -> Format.printf "Push %d" i
    | Add -> Format.printf "Add"
    | Roll i -> Format.printf "Roll %d" i
    | Apply -> Format.printf "Apply"
    | Form_Closure (num_ops, num_vals) ->
       Format.printf "Form_Closure (%d, %d)" num_ops num_vals in
  List.iter (fun x -> loop x; Format.printf "\n") lst

(* Pretty print stack value *)
let print_stack_val v =
  let rec loop v =
    match v with
      | Int n ->
        Format.printf "%d " n
      | Closure _ ->
        Format.printf "<fun> " in
  Format.printf "@[";
  loop v;
  Format.printf "@]"

(* Pretty print stack *)
let print_stack s = 
  Format.printf "top [ ";
  List.iter (fun x -> print_stack_val x) s;
  Format.printf "] bottom\n"

