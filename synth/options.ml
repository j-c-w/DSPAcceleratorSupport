exception OptionsException of string

type backend_target =
	| CXX

let backend_target_from_string str =
	match str with
	| Some("C++") -> CXX
	| None -> CXX
	| _ -> raise (OptionsException ("Unknown target "))

let get_compiler_cmd target =
    match target with
    | CXX -> "g++"

type options = {
	(* Generic configuration *)
	target: backend_target; (* Language target *)
	execution_folder: string; (* Where to keep the executables for testing *)
    compiler_cmd: string;

	(* Testing configuration *)
	number_of_tests: int;

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
    debug_generate_code: bool;
	debug_build_code: bool;
	debug_generate_io_tests: bool;

	(* GIR passes debug.  *)
	debug_gir_topology_sort: bool;

	(* SType passes debug.  *)
	debug_synth_topology: bool;

	(* Generic debug *)
	print_synthesizer_numbers: bool;
}
