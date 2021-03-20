open Core_kernel;;
open Spec_definition;;
open Skeleton_definition;;

(* NOTE: In GIR everything must be assigned to at most once.
This can be in a loop, etc. but must be once to allow
for topology computations.  *)
(* UNTIL: everything is put in the program type -- at that point
it is no longer SSA (since we need the underlying
C to not be SSA).  *)

type gir_name =
	Name of string

type function_ref = FunctionRef of gir_name
(* Needs a list name and the variable that we are considering. *)
type expression =
	| VariableReference of variable_reference
	| FunctionCall of function_ref * varlist
    (* Lookup variable_reference in the list of pairs of synthvalues.  *)
    | GIRMap of gir_name * (synth_value * synth_value) list
and variable_reference =
	| Variable of gir_name
	| MemberReference of variable_reference * gir_name
	(* Variable * index variable *)
	| IndexReference of variable_reference * expression
and rvalue =
	| Expression of expression

and lvalue =
    | LVariable of variable_reference

and varlist =
    | VariableList of variable_reference list

and gir =
	| Definition of gir_name
	| Sequence of gir list
	(* This can eitehr assign lists to lists, of variables to
	   variables.  *)
	| Assignment of lvalue * rvalue
	(* Body, induction variable name, loop max value *)
    | LoopOver of gir * gir_name * variable_reference
	| Expression of expression
    (* Function definition, has a name, a list of args, and a body.  *)
    | FunctionDef of gir_name * (gir_name list) * gir * ((string, synth_type) Hashtbl.t)
    | Return of gir_name
	| EmptyGIR
	(* (why?) Todo --- add a lambda here *)
