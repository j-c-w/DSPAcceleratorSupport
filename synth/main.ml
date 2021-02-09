open Generatec
open Cmdliner
open Gir
open Parse_iospec
open Parse_classmap
open Parse_api
open Synthesize
open Api
open Options

let main options classspec_file iospec_file api_file  =
	let classspec = load_classmap classspec_file in
    let iospec = load_iospec options iospec_file in
    let api = load_target_api api_file in
    let synth_results = run_synthesis options classspec iospec api in
	Printf.printf "Synthesizing...\n";
	Printf.printf "json live in is ";
	Printf.printf "%s\n" iospec_file;
	Printf.printf "Done!\n";;

let optswrapper classspec_file iospec_file api_file dump_skeletons 
        debug_generate_skeletons dump_assigned_dimensions debug_assign_dimensions 
		debug_load debug_generate_gir dump_generate_gir 
		debug_generate_program dump_generate_program 
		print_synth_program_nums target =
    (* First make the options object, then call the normal main function.  *)
    let options = {
        target = (backend_target_from_string target);

		dump_assigned_dimensions = dump_assigned_dimensions;
        dump_skeletons = dump_skeletons;
		dump_generate_gir = dump_generate_gir;
		dump_generate_program = dump_generate_program;

		debug_load = debug_load;
		debug_generate_skeletons = debug_generate_skeletons;
        debug_assign_dimensions = debug_assign_dimensions;
		debug_generate_gir = debug_generate_gir;
		debug_generate_program = debug_generate_program;

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

(* Generic debug flags *)
let print_synth_option_numbers =
	let doc = "Print number of options the synthesizer has at each stage" in
	Arg.(value & flag & info ["print-synthesizer-numbers"] ~docv:"PrintSkeletonNumbers" ~doc)

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

(* Debug flags *)
let info =
	let doc = "Synthesize support for hardware accelerators" in
	Term.info "synth" ~doc

(* This command line parser is a shitshow, or I don't know how to use it.
   In any case, this has to stay in the right order.  *)
let args_t = Term.(const optswrapper $ classspec $ iospec $ apispec $ dump_skeletons $
    debug_generate_skeletons $ dump_assigned_dimensions $ debug_assign_dimensions $ debug_load $
	debug_generate_gir $ dump_generate_gir $ debug_generate_program $ dump_generate_program
	$ print_synth_option_numbers $ target)
let () = Term.exit @@ Term.eval (args_t, info)
