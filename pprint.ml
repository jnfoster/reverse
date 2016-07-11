open Ast
open State
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

let print_lambda p xs e =
    Format.printf "@[<2>(lambda";
    List.iter (fun x -> Format.printf " "; print_ident x) xs;
    Format.printf ".@ ";
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
      | Binop (Plus, l,r) -> print_binop loop "+" l r
      | Binop (Minus, l,r) -> print_binop loop "-" l r
      | Binop (Multiply, l,r) -> print_binop loop "*" l r
      | Binop (Divide, l,r) -> print_binop loop "/" l r
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

let print_state s = 
    let print_binding (var, value) =
        Format.printf "(%s, " var;
        print_val value;
        Format.printf ")" in
    List.iter print_binding (State.bindings s)

(* Pretty print program *)
let rec print_instrs (lst: program) =
  let rec loop inst =
    match inst with
    | Skip f -> 
            let () = Format.printf "Skip---\n" in f ()
    | Push i -> Format.printf "Push %d" i
    | Add -> Format.printf "Add"
    | Subt -> Format.printf "Subt"
    | Mult -> Format.printf "Mult"
    | Div -> Format.printf "Div"
    | Roll i -> Format.printf "Roll %d" i
    | Unroll i -> Format.printf "Unroll %d" i
    | MultiApply i -> Format.printf "MultiApply %d" i
    | Form_Closure (num_ops, num_vals) ->
       Format.printf "Form_Closure (%d, %d)" num_ops num_vals in
  List.iter (fun x -> loop x; Format.printf "\n") lst

(* Pretty print stack *)
let print_stack_repr s = 
    let rec print_repr = function
        | Const -> Format.printf "Const"
        | Stack_Var v -> Format.printf "Var %s" v in
  Format.printf "top [ ";
  List.iter (fun x -> print_repr x; Format.printf "\n") s;
  Format.printf "] bottom\n"


(* Pretty print stack value *)
let print_stack_val v =
  let rec loop v =
    match v with
      | Stack_Int n ->
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

