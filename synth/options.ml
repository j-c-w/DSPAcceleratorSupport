type options = {
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
