open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Run_definition;;
open Options;;

exception SparsityException of string

let generate_results_for (opts: options) (iospec: iospec) inp_files =
	(* Perhaps this should be parallelized? *)
	let progexec = iospec.execcmd in
	let results = Utils.parmap opts (fun infile ->
        let outfile = infile ^ "_result.json" in
		let timeout = (string_of_int opts.execution_timeout) in
        let runcmd = "timeout " ^ timeout ^ " " ^ progexec ^ " " ^ infile ^ " " ^ outfile in
		let () = if opts.debug_iospec_manipulator then
			let () = Printf.printf "Runcmd is %s\n%!" (runcmd) in
			()
		else () in
        (* TODO -- Need to have a timeout here.  *)
        let res = Sys.command runcmd in
		let () = if opts.debug_iospec_manipulator then
			let () = Printf.printf "Finished with result: %d\n" (res) in
			() else ()
		in
        if res <> 0 then
            RunFailure
        else
            RunSuccess(outfile)
	) inp_files
	in
	let success_count = List.count results (fun r -> match r with
		| RunFailure -> false
		| RunSuccess(_) -> true
	) in
	if success_count > 0 then
		Some(results)
	else
		(* Can't have no results! *)
		None

let compute_default_results opts iospec inp_files =
	List.map inp_files (fun inp_file_set ->
		let results = generate_results_for opts iospec inp_file_set in
		match results with
		| Some(r) -> r
		| None ->
				raise (SparsityException "Can't find any working inputs to user code: likely too sparse (try more inputs?)")
	)
