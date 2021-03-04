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
	(* Allow testing of a single generated test only.  *)
	only_test: string option;

	(* Speedup configs to enable partial runs during debugging. *)
	skip_build: bool;
	stop_before_build: bool;

	(* IR Dumps *)
	dump_assigned_dimensions: bool;
    dump_skeletons: bool;
	dump_generate_gir: bool;
	dump_generate_program: bool;
	dump_test_results: bool;

	(* Pass debug *)
	debug_load: bool;
	debug_assign_dimensions: bool;
	debug_generate_skeletons: bool;
	debug_generate_gir: bool;
    debug_generate_program: bool;
    debug_generate_code: bool;
	debug_build_code: bool;
	debug_generate_io_tests: bool;
    debug_iospec_manipulator: bool;
	debug_test: bool;

	(* GIR passes debug.  *)
	debug_gir_topology_sort: bool;
    debug_gir_reduce: bool;

	(* SType passes debug.  *)
	debug_synth_topology: bool;

	(* Skeleton passes debug.  *)
	debug_skeleton_flatten: bool;

	(* Generic debug *)
	print_synthesizer_numbers: bool;
}

(* this is a dirty hack --- fix the multiple frontends
   one opt parser issue is a better way to deal with this. *)
let default_options = {
	(* Generic configuration *)
	target = CXX;
	execution_folder = "synth_temps";
	compiler_cmd = "g++";

	(* Testing configuration *)
	number_of_tests = 100;
	(* Allow testing of a single test only.  *)
	only_test = None;

	(* Speedup configs to enable partial runs during debugging. *)
	skip_build = false;
	stop_before_build = false;

	(* IR Dumps *)
	dump_assigned_dimensions = false;
	dump_skeletons = false;
	dump_generate_gir = false;
	dump_generate_program = false;
	dump_test_results = false;

	(* Pass debug *)
	debug_load = false;
	debug_generate_skeletons = false;
	debug_assign_dimensions = false;
	debug_generate_gir = false;
	debug_generate_program = false;
	debug_generate_code = false;
	debug_build_code = false;
	debug_generate_io_tests = false;
    debug_iospec_manipulator = false;
	debug_test = false;

	(* GIR passes debug.  *)
	debug_gir_topology_sort = false;
	debug_gir_reduce = false;

	(* SType passes debug.  *)
	debug_synth_topology = false;

	(* Skeleton passes debug.  *)
	debug_skeleton_flatten = false;

	(* Generic debug *)
	print_synthesizer_numbers = false;
}
