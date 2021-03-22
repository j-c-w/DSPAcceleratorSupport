open Core_kernel;;
open Synthesizer_interface;;
open Program;;
open Options;;
open Gir_utils;;

(* Expect one set of IO pairs for each program to consider.  *)
let run_post_synthesis options classmap iospec apispec programs io_files =
    let prog_io_pairs = List.zip_exn programs io_files in
    List.map prog_io_pairs (fun ((prog: program), (iopairs, passed)) ->
        if passed then
            let () =
                if options.dump_behavioural_synth then
                    Printf.printf "No behavioural synth required!\n"
                else () in
			{
                prog with
				post_behavioural = None
			}, false
        else
            let post_program = synthesize_post options classmap iospec apispec prog iopairs in
            let () = if options.dump_behavioural_synth then
                Printf.printf "Behavioural synth used is %s\n" (
                    match post_program with
                    | Some(x) -> gir_to_string x.program
                    | None -> "None (failed)"
                )
            else () in
			{
                prog with
				post_behavioural = post_program
            }, true
        )
