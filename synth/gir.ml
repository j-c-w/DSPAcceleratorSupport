open Core_kernel;;
open Spec_definition;;

(* NOTE: In GIR everything must be assigned to at most once.
This can be in a loop, etc. but must be once to allow
for topology computations.  *)
(* UNTIL: everything is put in the program type -- at that point
it is no longer SSA (since we need the underlying
C to not be SSA).  *)

type function_ref = FunctionRef of name_reference
(* Needs a list name and the variable that we are considering. *)
type expression =
	| VariableReference of name_reference
	| ListIndex of expression * expression
	| FunctionCall of function_ref * varlist

and rvalue =
	| Expression of expression

and lvalue =
    | LVariable of name_reference
	(* Variable * index variable *)
	| LIndex of lvalue * expression

and varlist =
    | VariableList of name_reference list

and gir =
	| Definition of name_reference
	| Sequence of gir list
	(* This can eitehr assign lists to lists, of variables to
	   variables.  *)
	| Assignment of lvalue * rvalue
	(* Body, induction variable name, loop max value *)
    | LoopOver of gir * name_reference * name_reference
	| Expression of expression
	| EmptyGIR
	(* (why?) Todo --- add a lambda here *)

(* This should be a list of live-in variables, the function
	and then the live out varaibles. *)
type program = {
    in_variables: string list;
    gir: gir;
    out_variables: string list;
    typemap: (string, synth_type) Hashtbl.t
}
