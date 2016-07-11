open Ast

(** Stack machine instructions *)
type operation = Push of int | Add | Subt | Mult | Div | Roll of int | MultiApply of int | Unroll of int | Form_Closure of int * int | Skip of (unit -> unit)
type program = operation list

(** Stack machine values *)
type stack_value = Stack_Int of int | Closure of int * program
and stack = stack_value list

(** Used to maintain a representation of the stack *)
type value_repr = Const | Stack_Var of var
type stack_repr = value_repr list

(** Maintains history of applying non-injective functions *)
type history_tape = int list

(** Tuple of program input, stack, history_tape, program history *)
type machine_state = program * stack * history_tape * program
