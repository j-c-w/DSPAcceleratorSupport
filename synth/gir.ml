open Spec_definition;;

type classname = Classname of string
type function_ref = FunctionRef of name_reference
(* Needs a list name and the variable that we are considering. *)
type expression =
	| VariableReference of name_reference
	| ListIndex of name_reference * expression
	| ClassDeref of classname * name_reference
	| FunctionCall of function_ref * varlist

and rvalue =
	| Expression of expression
    | RReference of name_reference

and lvalue =
    | LVariable of name_reference

and varlist =
    | VariableList of name_reference list

and gir =
	| Sequence of gir list
	(* This can eitehr assign lists to lists, of variables to
	   variables.  *)
	| Assignment of lvalue * rvalue
	(* Body, induction variable. *)
    | LoopOver of gir * name_reference
	| Expression of expression
	| EmptyGIR
	(* Todo --- add a lambda here *)

(* This should be a list of live-in variables, the function
	and then the live out varaibles. *)
type program = {
    in_variables: name_reference list;
    gir: gir;
    out_variables: name_reference list;
    typemap: (string, synth_type) Hashtbl.t
}
