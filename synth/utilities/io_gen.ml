open Cmdliner;;
open Generate_io_tests;;
open Parse_iospec;;
open Parse_api;;
open Parse_classmap;;
open Spec_definition;;
open Spec_utils;;
open Options;;
open Gir;;
open Program;;

let () = Printexc.record_backtrace true;;

let main iospec_file num_tests output_folder max_string_size seed debug_generate_io_tests
        array_length_threshold =
	let _ = Random.init seed in
	let options = { default_options with
		number_of_tests = num_tests;
		execution_folder = output_folder;
		(* We could use parmap, but just generating a few IO examples it probably isn't
		   required.  It can obscure error messages (e.g. if there are ambiguities
		   in the iospec used.) *)
		use_parmap = false;
		debug_generate_io_tests = debug_generate_io_tests;
        max_string_size = max_string_size;
		(* Since we are just doing 
		generation, print arnings if we abort
		due to repeatedly allocating too much data.  *)
		print_array_length_warnings = true;
        array_length_threshold = array_length_threshold;
	} in
	let iospec, iotypemap, classspec = load_iospec options iospec_file in
	let empty_alignmenttbl = empty_typemap () in

	(* Construct the program from the input IO map.  We assume
	   that there is no ambiguity in the iospec.   That is, we
	   do not run type inference or length inference --- we expect
	   that the IO map has been anontated as such.  *)
	let rec typemap = {
		variable_map = iotypemap;
		classmap = classspec;
		alignment_map = empty_alignmenttbl;
		original_typemap = Some(typemap);
	} in
	let base_program = {
		funargs = iospec.funargs;
		livein = iospec.livein;
		liveout = iospec.liveout;
		gir = EmptyGIR;
		typemap = typemap;
		range_checker = None;
		post_behavioural = None;
		returnvar = iospec.returnvar;
		user_funname = "TODO";
		generated_funname = iospec.funname;
		api_funname = "TODO";
		fundefs = [];
		inputmap = iospec.rangemap; (* TODO --- should really be intersection of range and valid? *)
		original_pairs = None;
		allocated_variables = [];
	} in
	let _ = generate_io_tests options iospec [base_program] in
	(* Function needs to return unit or we get very convoluted errors *)
	()

let iospec =
	let doc = "IO Spec for function" in
	Arg.(required & pos 0 (some string) None & info [] ~docv:"IOSpec" ~doc)

let number =
	let doc = "Number of Examples" in
	Arg.(required & pos 1 (some int) None & info [] ~docv:"Number" ~doc)
let max_string_size =
	let doc = "Max size of strings to generate as part of IO test" in
	Arg.(value & opt int 100 & info ["max-string-size"] ~docv:"MaxStringSize" ~doc)
let array_length_threshold =
    let doc = "max valid size of allocated arrays" in
    Arg.(value & opt int 10000 & info ["array-length-threshold"] ~docv:"ArrayLengthThreshold" ~doc)
let seed =
	let doc = "Random Seed" in
	Arg.(value & opt int 0 & info ["random-seed"] ~docv:"RandomSeed" ~doc)
let debug_generate_io_tests =
	let doc = "Debug generate IO tests" in
	Arg.(value & flag & info ["debug-generate-io"] ~docv:"DebugGenerateIO" ~doc)

let destfolder =
	let doc = "Distination folder" in
	Arg.(required & pos 2 (some string) None & info [] ~docv:"Destination" ~doc)

let info =
	let doc = "Generate IO Examples" in
	Cmd.info "IOGen" ~doc

let args_t = Term.(const main $ iospec $ number $ destfolder $ max_string_size $ seed $ debug_generate_io_tests $ array_length_threshold)

let () = Stdlib.exit @@ Cmd.eval (Cmd.v info args_t)
