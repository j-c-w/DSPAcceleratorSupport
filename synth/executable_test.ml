open Core_kernel;;
open Yojson;;
open Yojson.Basic.Util;;
open Run_definition;;
open Options;;

(* Largely, we assume taht j1 and j2 have the same members
this will sometimes crash and sometimes spuriously go true
if they do not.  *)
let rec compare_jsons j1 j2 =
    let j1_members = keys j1 in
    List.for_all j1_members (fun mem ->
        match (j1 |> member mem, j2 |> member mem) with
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
                              (name1 = name2) && (compare_jsons json1 json2)
                          ) in
						  r
                  | Unequal_lengths -> false
                  )
          | `Bool(b1), `Bool(b2) ->
                  b1 = b2
          | `Float(f1), `Float(f2) ->
                  (* TODO --- fix *)
                  (f1 <= (f2 +. 0.001)) && (f1 >= (f2 -. 0.001))
          | `Int(i1), `Int(i2) ->
                  i1 = i2
          | `List(l1), `List(l2) ->
                  ((List.length l1) = (List.length l2)) &&
                  List.for_all (List.zip_exn l1 l2) (fun (i1, i2) ->
                      compare_jsons i1 i2
                  )
          | `Null, `Null -> true
          | `String(s1), `String(s2) ->
                  s1 = s2
          | _ -> false

    )

let compare_outputs f1 f2 =
    (* Open both in Yojson and parse. *)
    let f1_json = Yojson.Basic.from_file f1 in
    let f2_json = Yojson.Basic.from_file f2 in
    compare_jsons f1_json f2_json

let find_working_code (options:options) generated_executables generated_io_tests correct_answer_files =
	(* TODO --- perhaps a parmap here?  Need to make sure the output files don't overlap if so. *)
    (* This might also end up being limited by disk performance.  Perhaps using
    a ramdisk would help? *)
	let tests_and_results = List.zip_exn generated_io_tests correct_answer_files in
	List.map generated_executables (fun execname ->
		(* We could do something like 'for_all', but we don't
		really want to run every test for every executable ---
		most are going to fail immediately.  *)
        let ()  = if options.debug_test then
            Printf.printf "Starting tests for executable %s\n" execname
        else () in
		let res = List.take_while tests_and_results (fun (testin, testout) ->
			(* Get an output name for this test.  *)
			let experiment_outname = testin ^ "_outtmp.json" in
			(* Run the program on this test input.  *)
			(* TODO --- maybe we should time this out?  Less
			clear whether we need that here than we did with
			the user code (where we also don't timeout) *)
			let result = Sys.command (execname ^ " " ^ testin ^ " " ^ experiment_outname) in
			let same_res = match testout with
			| RunFailure -> 
				(* We could be a bit smarter than this.  Anyway,
				I'm hoping not to deal with too many failures,
				they're more of an edge case(? famous last words). *)
				result <> 0
			| RunSuccess(outf) ->
				compare_outputs experiment_outname outf
			in
			(* Delete the temp output file from this experiment *)
			let delresult = if not options.dump_test_results then
                (* only delete if we dont' want to keep the test results.  *)
                Sys.command ("rm " ^ experiment_outname)
            else 0 in
			let () = assert (delresult = 0) in
            same_res
		) in
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
	
