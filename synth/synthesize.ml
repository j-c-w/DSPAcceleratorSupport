open Core_kernel;;
open Spec_definition;;
open Assign_dimensions;;
open Generate_gir;;
open Generate_programs;;
open Generate_code;;
open Build_code;;
open Generate_io_tests;;
open Skeleton
open Iospec_manipulator;;
open Options;;

exception TypeException of string

(* TODO --- Find a better way to deal with bounds.  *)
let maxarraylength = 100;;
let floatmax = 100.0;;
let intmax = 1000;;

let run_synthesis (opts:options) (classmap: (string, structure_metadata) Hashtbl.t) (iospec: iospec) (api: apispec) =
	(* Assign possible dimension equalities between vector types.  *)
	(* This updates the type ref tables in place, so no reassigns needed.  *)
	let _ = assign_dimensions opts classmap iospec.typemap iospec.livein in
	(* We currently also do the same for the API, although it is plenty
	   possible to ask the accelerator designer to specify this for the API.
	   Bit more scalable if we dont :) *)
	let _ = assign_dimensions opts classmap api.typemap api.livein in
    (* Generate the possible skeletons to consider *)
    let skeleton_pairs = generate_skeleton_pairs opts classmap iospec api in
	let () = if opts.dump_skeletons = true then
		Printf.printf "%s%s\n" "Skeletons are " (skeleton_pairs_to_string skeleton_pairs)
	else
		() in
    let () = if opts.print_synthesizer_numbers then
        Printf.printf "Number of skeletons generated is %d\n" (List.length skeleton_pairs)
    else () in
	(* Do some lenvar expansion to avoid incompatible lenvar
	   at the next stages? *)
	(* Should also do some assignment merging here, e.g. if
		we have structs that are exactly the same type.  *)
	(* Do some internal simulation on the pairs? *)
	(* Generate the actual conversion functions between the code pairs *)
	let conversion_functions = generate_gir opts classmap iospec api skeleton_pairs in
    let () = if opts.print_synthesizer_numbers then
        Printf.printf "Number of conversion pairs generated is %d\n" (List.length conversion_functions)
    else () in
	(* Generate program from the pre/post convsersion function pairs. *)
	let programs = generate_programs opts classmap iospec api conversion_functions in
    let () = if opts.print_synthesizer_numbers then
        Printf.printf "Number of programs from these pairs is %d\n" (List.length programs)
    else () in
	(* Do some opts? *)
	(* Generate some code.  *)
	let generated_code = generate_code opts classmap api iospec programs in
	let () = if opts.print_synthesizer_numbers then
		Printf.printf "Number of codes generated is %d\n" (List.length generated_code)
	else () in
	(* Build the code *)
	let code_files = build_code opts generated_code in
	(* Generate some I/O tests.  *)
	let io_tests = generate_io_tests opts classmap iospec in
	let () = if opts.print_synthesizer_numbers then
		Printf.printf "Number of IO tests generated is %d\n" (List.length io_tests)
	else () in
	(* Generate the 'correct' responses for the IO tests *)
	let real_response_files = generate_results_for opts iospec in
	(* Try the code until we find one that works.  *)
	(* let working_codes = find_working_code generated_code generated_io_tests in *)
	(* Do some opts? *)
	()
;;
