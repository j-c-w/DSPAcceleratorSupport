open Core_kernel;;
open Spec_definition;;
open Gir;;

type post_behavioural_program = {
	program: string
}

(* This should be a list of live-in variables, the function
	and then the live out varaibles. *)
type program = {
    in_variables: string list;
    gir: gir;
    out_variables: string list;
	post_behavioural: post_behavioural_program option;
    typemap: (string, synth_type) Hashtbl.t;
	returnvar: string option;
	lenvar_bindings: (string, dimension_type) Hashtbl.t;
	fundefs: gir list
}

type gir_pair = {
	pre: gir;
	post: gir;
	(* Which lenvar assignments are being used by this
	   for each array type? *)
	lenvar_bindings: (string, dimension_type) Hashtbl.t;
	(* What helper functions are required? *)
	fundefs: gir list;
}
