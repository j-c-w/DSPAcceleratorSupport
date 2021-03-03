open Cmdliner
open Gir
open Parse_iospec
open Parse_classmap
open Parse_api
open Synthesize
open Api
open Options

let () = Printexc.record_backtrace true;;

let main options classspec_file iospec_file api_file  =
	let () = Printf.printf "Loading specifications...\n" in
	let classspec = load_classmap classspec_file in
    let iospec = load_iospec options classspec iospec_file in
    let api = load_target_api classspec api_file in
	let () = Printf.printf "Synthesizing...\n" in
    let _ = run_synthesis options classspec iospec api in
	Printf.printf "json live in is ";
	Printf.printf "%s\n" iospec_file;
	Printf.printf "Done!\n";;

let optswrapper classspec_file iospec_file api_file dump_skeletons
        debug_generate_skeletons dump_assigned_dimensions debug_assign_dimensions
		debug_load debug_generate_gir dump_generate_gir
		debug_generate_program dump_generate_program
		print_synth_program_nums target execution_folder
		compiler_cmd debug_build_code debug_gir_topology_sort
        debug_generate_code number_of_tests debug_generate_io_tests
        debug_synth_topology debug_iospec_manipulator
		skip_build dump_test_results debug_test debug_skeleton_flatten
		stop_before_build only_test  =
    (* First make the options object, then call the normal main function.  *)
    let target_type = backend_target_from_string target in
    let options = {
        target = target_type;
		execution_folder = execution_folder;
		compiler_cmd = (match compiler_cmd with
		| Some(cmd) -> cmd
		| None -> get_compiler_cmd target_type
        );

        number_of_tests = number_of_tests;

		skip_build = skip_build;
		stop_before_build = stop_before_build;
		
		only_test = only_test;

		dump_assigned_dimensions = dump_assigned_dimensions;
        dump_skeletons = dump_skeletons;
		dump_generate_gir = dump_generate_gir;
		dump_generate_program = dump_generate_program;
		dump_test_results = dump_test_results;

		debug_load = debug_load;
		debug_generate_skeletons = debug_generate_skeletons;
        debug_assign_dimensions = debug_assign_dimensions;
		debug_generate_gir = debug_generate_gir;
		debug_generate_program = debug_generate_program;
		debug_generate_code = debug_generate_code;
		debug_build_code = debug_build_code;
        debug_generate_io_tests = debug_generate_io_tests;
		debug_iospec_manipulator = debug_iospec_manipulator;
		debug_test = debug_test;

		debug_gir_topology_sort = debug_gir_topology_sort;

        debug_synth_topology = debug_synth_topology;
		
		debug_skeleton_flatten = debug_skeleton_flatten;

		print_synthesizer_numbers = print_synth_program_nums;
    }
    in
    main options classspec_file iospec_file api_file

(* Deal with the commandline arguments. *)
(* Required positional args *)
let classspec =
	let doc = "Class Specifiction for the program" in
	Arg.(required & pos 0 (some string) None & info [] ~docv:"ClassSpec" ~doc)

let iospec =
	let doc = "IO Specification for the Function" in
	Arg.(required & pos 1 (some string) None & info [] ~docv:"IOSpec" ~doc)

let apispec =
    let doc = "IO Specification for the target API" in
    Arg.(required & pos 2 (some string) None & info [] ~docv:"APISpec" ~doc)

(* Configuration flags *)
let target =
	let doc = "Target generation language (default C++). Must be one of [C++] right now." in
	Arg.(value & opt (some string) None & info ["target"] ~docv:"Target" ~doc)
let execution_folder =
	let doc = "Folder in which to build/run all the potential programs" in
	Arg.(value & opt string "synthethizer_temps" & info ["execution-folder"] ~docv:"ExecFolder" ~doc)
let compiler_cmd =
	let doc = "Build compiler command for internal IO analysis (default g++)" in
	Arg.(value & opt (some string) None & info ["compiler-command"] ~docv:"CompilerCommand" ~doc)

(* Testing configuration flags.  *)
let number_of_tests =
    let doc = "Max number of tests to generate.  " in
    Arg.(value & opt int 10 & info ["number-of-tests"] ~docv:"NumberOfTests" ~doc)
let only_test =
	let doc = "Only test the testcase with the name specified here
	(usually to see the output of that test)" in
	Arg.(value & opt (some string) None & info ["only-test"] ~docv:"OnlyTest" ~doc)

(* Generic debug flags *)
let print_synth_option_numbers =
	let doc = "Print number of options the synthesizer has at each stage" in
	Arg.(value & flag & info ["print-synthesizer-numbers"] ~docv:"PrintSkeletonNumbers" ~doc)

(* Debug flags to be used with care. *)
let skip_build =
	let doc = "Only run testing and not generation. Debug only. " in
	Arg.(value & flag & info ["skip-build"] ~docv:"TestOnly" ~doc)
let stop_before_build =
	let doc = "Stop before building any candidate programs.  " in
	Arg.(value & flag & info ["stop-before-build"] ~docv:"StopBeforeBuild" ~doc)

(* Print IR flags *)
let dump_skeletons =
    let doc = "Dump skeletons" in
    Arg.(value & flag & info ["dump-skeletons"] ~docv:"DumpSkeletons" ~doc)
let dump_assigned_dimensions =
	let doc = "Dump assigned dimension variables" in
	Arg.(value & flag & info ["dump-dimensions"] ~docv:"DumpDimensions" ~doc)
let dump_generate_gir =
	let doc = "Dump GIR after generation but before program generation" in
	Arg.(value & flag & info ["dump-generate-gir"] ~docv:"DumpGenerateGIR" ~doc)
let dump_generate_program =
	let doc = "Dump Generated program in gir form" in
	Arg.(value & flag & info ["dump-generate-program"] ~docv:"DumpGenerateProg" ~doc)
let dump_test_results =
	let doc = "Dump the results of testing " in
	Arg.(value & flag & info ["dump-test-results"] ~docv:"DumpTestResults" ~doc)

(* Debug GIR manipulation passes.  *)
let debug_gir_topology_sort =
	let doc = "Debug GIR topology passes" in
	Arg.(value & flag & info ["debug-gir-topology"] ~docv:"DebugGIRTopo" ~doc)

(* debug stype passes.  *)
let debug_synth_topology =
    let doc = "Debug the synth topology sort pass" in
    Arg.(value & flag & info ["debug-synth-topology"] ~docv:"DebugSynthTopo" ~doc)
	
(* debug skeleton passes *)
let debug_skeleton_flatten =
	let doc = "Debug skeleton flatten pass" in
	Arg.(value & flag & info ["debug-skeleton-flatten"] ~docv:"DebugSkeletonFlatten" ~doc)

(* Debug pass internal flags *)
let debug_generate_gir =
	let doc = "Print debug information for generate_gir.ml" in
	Arg.(value & flag & info ["debug-generate-gir"] ~docv:"DebugGenGIR" ~doc)
let debug_generate_skeletons =
    let doc = "Print debug information for skeleton.ml" in
    Arg.(value & flag & info ["debug-skeletons"]  ~docv:"DebugSkeletons" ~doc)
let debug_assign_dimensions =
	let doc = "Print debug information for assign_dimensions.ml" in
	Arg.(value & flag & info ["debug-assign-dimensions"] ~docv:"DebugAssignDimensions" ~doc)
let debug_load =
	let doc = "Debug the loading pass" in
	Arg.(value & flag & info ["debug-load"] ~docv:"DebugLoad" ~doc)
let debug_generate_program =
	let doc = "Debug the generate program pass" in
	Arg.(value & flag & info ["debug-generate-program"] ~docv:"DebugGenProgram" ~doc)
let debug_generate_code =
	let doc = "Debug the generate code pass" in
	Arg.(value & flag & info ["debug-generate-code"] ~docv:"DebugGenCode" ~doc)
let debug_build_code =
	let doc = "Debug code building pass" in
	Arg.(value & flag & info ["debug-build-code"] ~docv:"DebugBuildCode" ~doc)
let debug_generate_io_tests =
    let doc = "Debug generation of IO tests " in
    Arg.(value & flag & info ["debug-generate-io-tests"] ~docv:"DebugGenerateIO" ~doc)
let debug_iospec_manipulator =
	let doc = "Debug iospec manipulator (e.g. running the generated tests over the tested API" in
	Arg.(value & flag & info ["debug-iospec-manipulator"] ~docv:"DebugIOSpecManip" ~doc)
let debug_test =
	let doc = "Debug the testing phase.  " in
	Arg.(value & flag & info ["debug-test"] ~docv:"DebugTest" ~doc)

(* Debug flags *)
let info =
	let doc = "Synthesize support for hardware accelerators" in
	Term.info "synth" ~doc

(* This command line parser is a shitshow, or I don't know how to use it.
   In any case, this has to stay in the right order.  *)
let args_t = Term.(const optswrapper $ classspec $ iospec $ apispec $ dump_skeletons $
    debug_generate_skeletons $ dump_assigned_dimensions $ debug_assign_dimensions $ debug_load $
	debug_generate_gir $ dump_generate_gir $ debug_generate_program $ dump_generate_program
	$ print_synth_option_numbers $ target $ execution_folder $ compiler_cmd $ debug_build_code $
	debug_gir_topology_sort $ debug_generate_code $ number_of_tests $ debug_generate_io_tests $
    debug_synth_topology $ debug_iospec_manipulator $ skip_build $ dump_test_results $ debug_test $
	debug_skeleton_flatten $ stop_before_build $ only_test)
let () = Term.exit @@ Term.eval (args_t, info)
