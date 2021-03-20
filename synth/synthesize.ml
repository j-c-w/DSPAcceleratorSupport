open Core_kernel;;
open Spec_definition;;
open Assign_dimensions;;
open Generate_gir;;
open Generate_programs;;
open Generate_code;;
open Build_code;;
open Generate_io_tests;;
open Skeleton;;
open Skeleton_utils;;
open Iospec_manipulator;;
open Executable_test;;
open Post_synthesis;;
open Options;;

exception TypeException of string

(* TODO --- Find a better way to deal with bounds.  *)
let maxarraylength = 100;;
let floatmax = 100.0;;
let intmax = 1000;;

(* This is a debug flag to make it easier to peer into
   what the post-synthesis passes are doing without
   being overloaded.  Also to make repeat synthesis
   from the same inputs much faster.  *)
let reduce_programs (opts:options) programs =
    let filtered_programs = match opts.only_test with
        | None -> programs
        | Some(filter) ->
                match List.nth programs filter with
				| None -> raise (OptionsException ("No program with number " ^ (string_of_int filter) ^ " found"))
				| Some(prog) -> [prog]
    in
    filtered_programs

let run_synthesis (opts:options) (classmap: (string, structure_metadata) Hashtbl.t) (iospec: iospec) (api: apispec) =
	(* Assign possible dimension equalities between vector types.  *)
	(* This updates the type ref tables in place, so no reassigns needed.  *)
	let () = if opts.print_synthesizer_numbers then
		Printf.printf "Starting synthesis!%!\n"
	else () in
	let _ = assign_dimensions opts classmap iospec.typemap (iospec.livein @ iospec.liveout) in
	(* We currently also do the same for the API, although it is plenty
	   possible to ask the accelerator designer to specify this for the API.
	   Bit more scalable if we dont :) *)
	let _ = assign_dimensions opts classmap api.typemap (api.livein @ api.liveout) in
	let () = if opts.print_synthesizer_numbers then
		let () = Printf.printf "Generated the dimensions%!\n" in
		()
	else () in
    (* Generate the possible skeletons to consider *)
    let skeleton_pairs = generate_skeleton_pairs opts classmap iospec api in
	let () = if opts.dump_skeletons then
		Printf.printf "%s%s\n" "Skeletons are%! " (flat_skeleton_pairs_to_string skeleton_pairs)
	else
		() in
    let () = if opts.print_synthesizer_numbers then
        Printf.printf "Number of skeletons generated is %d%!\n" (List.length skeleton_pairs)
    else () in
	(* Do some lenvar expansion to avoid incompatible lenvar
	   at the next stages? *)
	(* Should also do some assignment merging here, e.g. if
		we have structs that are exactly the same type.  *)
	(* Do some internal simulation on the pairs? *)
	(* Generate the actual conversion functions between the code pairs *)
	let conversion_functions = generate_gir opts classmap iospec api skeleton_pairs in
    let () = if opts.print_synthesizer_numbers then
        Printf.printf "Number of conversion pairs generated is %d%!\n" (List.length conversion_functions)
    else () in
	(* Generate program from the pre/post convsersion function pairs. *)
	let programs = generate_programs opts classmap iospec api conversion_functions in
    let () = if opts.print_synthesizer_numbers then
        Printf.printf "Number of programs from these pairs is %d%!\n" (List.length programs)
    else () in
    let reduced_programs = reduce_programs opts programs in
	(* Do some opts? *)
    (* Do some filtering pre-generation? *)
	(* Generate some code.  *)
	(* START ROUND 1 Of Tests *)
	let generated_code = generate_code opts classmap api iospec reduced_programs in
	let () = if opts.print_synthesizer_numbers then
		Printf.printf "Number of codes generated is %d%!\n" (List.length generated_code)
	else () in
	if opts.stop_before_build then
		()
	else (
	(* Build the code *)
	let code_files = build_code opts api generated_code in
	let () = if opts.print_synthesizer_numbers then
		Printf.printf "Number of codes built is %d\n" (List.length code_files)
	else () in
	(* Generate some I/O tests.  *)
	let io_tests = generate_io_tests opts classmap iospec in
	let () = if opts.print_synthesizer_numbers then
		Printf.printf "Number of IO tests generated is %d%!\n" (List.length io_tests)
	else () in
	(* Generate the 'correct' responses for the IO tests *)
	let real_response_files = generate_results_for opts iospec io_tests in
	(* Try the code until we find one that works.  *)
	let working_codes = find_working_code opts code_files io_tests real_response_files in
	(* END Round 1 of Tests *)

    (* Run post-synthesis *)
    let post_synthesis_program = run_post_synthesis opts classmap iospec api reduced_programs working_codes in
    (* TODO --- regenerate the code and output the working ones
        in an output file!. *)
	(* Do some opts? *)
    (* let () = print_working_code code_files working_codes in *)
    let () = Printf.printf "Done!\n" in
    ()
	)
;;
