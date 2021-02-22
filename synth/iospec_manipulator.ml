open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Run_definition;;
open Options;;

let generate_results_for (opts: options) (iospec: iospec) inp_files =
	(* Perhaps this should be parallelized? *)
	let progexec = iospec.execcmd in
	List.map inp_files (fun infile ->
        let outfile = infile ^ "_result.json" in
        let runcmd = progexec ^ " " ^ infile ^ " " ^ outfile in
		let () = if opts.debug_iospec_manipulator then
			Printf.printf "Runcmd is %s\n" (runcmd)
		else () in
        (* TODO -- Need to have a timeout here.  *)
        let res = Sys.command runcmd in
        if res <> 0 then
            RunFailure
        else
            RunSuccess(outfile)
	)
