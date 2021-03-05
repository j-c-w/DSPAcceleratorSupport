open Core_kernel;;
open Yojson;;
open Yojson.Basic.Util;;
open Run_definition;;
open Options;;

(* Largely, we assume taht j1 and j2 have the same members
this will sometimes crash and sometimes spuriously go true
if they do not.  *)
let rec compare_jsons options j1 j2 =
	(* let () = Printf.printf "JSON j1 is %s \n" (Yojson.Basic.pretty_to_string j1) in
	let () = Printf.printf "JSON j2 is %s \n" (Yojson.Basic.pretty_to_string j2) in *)
    let j1_members = keys j1 in
    List.for_all j1_members (fun mem ->
		compare_json_elts options (j1 |> member mem) (j2 |> member mem)
	)

and compare_json_elts options e1 e2 =
	let result = match (e1, e2) with
	  | `Assoc(njson_pairs1), `Assoc(njson_pairs2) ->
			  let sorted_p1 = List.sort njson_pairs1 (fun (s1, j1) -> fun (s2, j2) ->
				  String.compare s1 s2
			  ) in
			  let sorted_p2 = List.sort njson_pairs2 (fun (s1, j1) -> fun (s2, j2) ->
				  String.compare s1 s2
			  ) in (
			  match List.zip sorted_p1 sorted_p2 with
			  | Ok(ls) ->
					  let r = List.for_all ls (fun ((name1, json1), (name2, json2)) ->
						  ((String.compare name1 name2) = 0) && (compare_jsons options json1 json2)
					  ) in
					  r
			  | Unequal_lengths -> false
			  )
	  | `Bool(b1), `Bool(b2) ->
			  (Bool.compare b1 b2) = 0
	  | `Float(f1), `Float(f2) ->
			  (* Aim for error no bigger than a 10th of f2, should
			  	100% make this configurable.  *)
			  let thresh = (Float.abs (f2)) /. 10.0 in
			  ((Float.compare f1 (f2 +. thresh)) = -1) &&
			  ((Float.compare f1 (f2 -. thresh) = 1))
	  | `Int(i1), `Int(i2) ->
			  i1 = i2
	  | `List(l1), `List(l2) ->
			  ((List.length l1) = (List.length l2)) &&
			  (* TODO --- fix --- no except on unequal lengths, just false I think? *)
			  List.for_all (List.zip_exn l1 l2) (fun (i1, i2) ->
				  compare_json_elts options i1 i2
			  )
	  | `Null, `Null -> true
	  | `String(s1), `String(s2) ->
			  (String.compare s1 s2) = 0
	  | _ -> false
	in
	if result then
		result
	else
		let () = if options.debug_comparison then
			Printf.printf "Comparison between %s and %s returned false!"
                (Yojson.Basic.pretty_to_string e1)
                (Yojson.Basic.pretty_to_string e1)
		else () in
		result

let compare_outputs options f1 f2 =
    (* Open both in Yojson and parse. *)
    let f1_json = Yojson.Basic.from_file f1 in
    let f2_json = Yojson.Basic.from_file f2 in
    compare_jsons options f1_json f2_json

let find_working_code (options:options) generated_executables generated_io_tests correct_answer_files =
	(* TODO --- perhaps a parmap here?  Need to make sure the output files don't overlap if so. *)
    (* This might also end up being limited by disk performance.  Perhaps using
    a ramdisk would help? *)
	let tests_and_results = List.zip_exn generated_io_tests correct_answer_files in
	let () = if options.debug_test then
		let () = Printf.printf "Number of tests is %d\n" (List.length generated_executables) in
		() else () in
	List.map generated_executables (fun execname ->
		(* We could do something like 'for_all', but we don't
		really want to run every test for every executable ---
		most are going to fail immediately.  *)
        let ()  = if options.debug_test then
            Printf.printf "Starting tests for executable %s\n" execname
        else () in
		let res = Utils.map_while tests_and_results (fun (testin, testout) ->
			(* Get an output name for this test.  *)
			let experiment_outname = testin ^ "_outtmp.json" in
			(* Run the program on this test input.  *)
			(* TODO --- maybe we should time this out?  Less
			clear whether we need that here than we did with
			the user code (where we also don't timeout) *)
			let cmd = execname ^ " " ^ testin ^ " " ^ experiment_outname in
			let () = if options.debug_test then
				Printf.printf "Running test command %s\n" cmd
			else () in
			let result = Sys.command cmd in
			let same_res = match testout with
			| RunFailure -> 
				(* We could be a bit smarter than this.  Anyway,
				I'm hoping not to deal with too many failures,
				they're more of an edge case(? famous last words). *)
                if result = 0 then
                    Some((testin, testout))
                else
                    None
			| RunSuccess(outf) ->
					if result = 0 then
                        if compare_outputs options experiment_outname outf then
                            Some((testin, testout))
                        else
                            None
					else
						(* Run of accelerator failed --- this probably
						shouldn't have happened.  *)
						let () = Printf.printf "Warning: Accelerator failed on input: accelerator bounds should be specified for better performance. \n" in
                        None
			in
			(* Delete the temp output file from this experiment *)
			let delresult = if (result = 0) && (not options.dump_test_results) then
                (* only delete if we dont' want to keep the test results.  *)
                Sys.command ("rm " ^ experiment_outname)
            else 0 in
			let () = assert (delresult = 0) in
            let should_continue = match same_res with
                  | Some(x) -> true
                (* Still continue if we are testing everything.  *)
                  | None -> if options.all_tests then true else false
            in
            same_res, should_continue
		) in
        let res = List.filter_map res Utils.id in
        let () = Printf.printf "Passed up to %d tests out of %d\n"
            (List.length res) (List.length tests_and_results) in
		(* If we took everyting, then we're golden :) *)
		(List.length res) = (List.length tests_and_results)
	)

let print_working_code files working_list =
	let working_filenames = List.filter_map (List.zip_exn files working_list) (fun (f, w) ->
		if w then
			Some(f)
		else
			None
	) in
	let () = Printf.printf "Working tests are in the source files for executables %s\n"
	(String.concat ~sep:"," working_filenames) in
	let () = Printf.printf "There were %d in total" (List.length working_filenames) in
	()
	
