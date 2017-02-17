type var = string
exception UnboundVariable of var
exception IllformedProgram
exception IllformedHistory

(** Stack machine instructions *)

type binop_op      = Add | Subt | Mult | Div
type func_op       = Save_Function of var * int | Form_Closure of var | Apply
type pair_op       = 
    (* tuple instructions *)
    | Construct_Tuple of int | Deconstruct_Tuple of int
    (* roll instructions *)
    | Roll of int | Unroll of int

type instruction   = 
    | Binop_Op of binop_op 
    | Func_Op  of func_op
    | Pair_Op  of pair_op
    | Push     of int
    | Skip     of (unit -> unit)

type program       = instruction list

(** Stack machine values *)

type tuple         = stack_value list
and  stack_value   = Stack_Int of int | Tuple of tuple | Closure of program * tuple
and  stack         = stack_value list

(** Used to maintain a representation of the stack *)
type value_repr    = Const | Stack_Var of var
type stack_repr    = value_repr list

(** Function stack **)
type fun_stack     = (var * program) list

(** Maintains history of applying non-injective functions *)
type history_tape  = int list

type machine       = program * stack * fun_stack

type record_tape   = history_tape * program
