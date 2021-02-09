exception OptionsException of string

type backend_target =
	| CXX

let backend_target_from_string str =
	match str with
	| "C++" -> CXX
	| _ -> raise (OptionsException ("Unknown target " ^ str))

type options = {
	(* Configuration *)
	target: backend_target;

	(* IR Dumps *)
	dump_assigned_dimensions: bool;
    dump_skeletons: bool;
	dump_generate_gir: bool;
	dump_generate_program: bool;

	(* Pass debug *)
	debug_load: bool;
	debug_assign_dimensions: bool;
	debug_generate_skeletons: bool;
	debug_generate_gir: bool;
    debug_generate_program: bool;

	(* Generic debug *)
	print_synthesizer_numbers: bool;
}
