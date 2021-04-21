open Core_kernel;;
open Spec_definition;;
open Gir;;
open Range_definition;;

type post_behavioural_program = {
	(* Any includes required for the generated program.  *)
	includes: string list;
	program: gir
}

type range_program = {
	condition: conditional
}

(* This should be a list of live-in variables, the function
	and then the live out varaibles. *)
type program = {
    in_variables: string list;
    gir: gir;
    out_variables: string list;
	range_checker: range_program option;
	post_behavioural: post_behavioural_program option;
    typemap: (string, synth_type) Hashtbl.t;
	inputmap: (string, range_set) Hashtbl.t;
	returnvar: string option;
	user_funname: string;
	generated_funname: string;
	api_funname: string;
	lenvar_bindings: (string, dimension_type) Hashtbl.t;
	fundefs: gir list
}

type gir_pair = {
	pre: gir;
	post: gir;
	(* Which input ranges are good to test this? *)
	inputmap: (string, range_set) Hashtbl.t;
	(* Which lenvar assignments are being used by this
	   for each array type? *)
	lenvar_bindings: (string, dimension_type) Hashtbl.t;
	(* What helper functions are required? *)
	fundefs: gir list;
	range_checker: range_program option;
}
