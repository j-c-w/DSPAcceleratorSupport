type programtype =
	| List
	| Int
	| Float
type typemap = (string, programtype) Hashtbl.t

type classname = Classname of string
type function_ref = FunctionRef of string
type variable = Variable of string
(* Needs a list name and the variable that we are considering. *)
	| ListVariable of string * variable
type variables =
	| EmptyVariableList
	| VariableList of variable * variables
type expression =
	| VariableReference of variable
	| ListIndex of variable * expression
	| ClassDeref of classname * variable

type gir =
	| Sequence of gir * gir
	(* This can eitehr assign lists to lists, of variables to
	   variables.  *)
	| Assignment of variable * variable * gir
	| Expression of expression
	| FunctionCall of function_ref * variables
	| EmptyGIR
	(* Todo --- add a lambda here *)

(* This should be a list of live-in variables, the function
	and then the live out varaibles. *)
type program = Program of variables * gir * variables * typemap
