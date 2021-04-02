open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Skeleton_definition;;
open Skeleton_utils;;
open Utils;;
open Options;;

let filter_dimvar_set dms = 
    (* We don't need to have the same dimension with
    the same source var for multiple targets.  *)
	(* Same thing with the to varaibles :) *)
    let flookup = Hashtbl.create (module String) in
	let tlookup = Hashtbl.create (module String) in
	let tbllookup_set = fun (tbl, n) ->
		let str = (name_reference_to_string n) in
		let already_mapped = Hashtbl.find tbl str in
		let () = Hashtbl.set tbl str true in
		match already_mapped with
			| Some(n) -> false
			| None -> true
	in
    let filtered = List.filter dms (fun dm ->
        match dm with
        | DimvarOneDimension(ExactVarMatch(f, t)) -> (
			(tbllookup_set (flookup, f)) && (tbllookup_set (tlookup, t))
		)
        (* TODO -- do we also need to do some filtering here? *)
		| DimvarOneDimension(ConstantMatch(f)) -> true
    )
    in
    let unique_dimvar_set = Utils.remove_duplicates dimvar_equal filtered in
    unique_dimvar_set

(* No variables assigned from more than once.  *)
let no_multiplie_cloning_check (skel: skeleton_type_binding) =
    let assigned_from = Hashtbl.create (module String) in
    let () = ignore(
        List.map skel.bindings (fun bind ->
            List.map bind.fromvars_index_nesting (fun vbind ->
                match vbind with
                | AssignConstant(c) -> ()
                | AssignVariable(indnest) ->
                    (* Get the indnesst string *)
                    let indnest_string = name_reference_to_string (StructName(indnest)) in
                    let existing_count = Hashtbl.find assigned_from indnest_string in
                    let newvar = match existing_count with
                    | None -> 1
                    | Some(x) -> x + 1
                    in
                    let _ = Hashtbl.set assigned_from indnest_string newvar in
                    ()
            )
        )
    ) in
    (* make sure that all the variables are used only once.
    Could update this to allow variables being used in
    multidefs to be used more than once, but that is a task
    for anohter day.  *)
    List.for_all (Hashtbl.data assigned_from) (fun v -> v = 1)

let rec build_whole_arnm_internal arnms =
	match arnms with
	| [] -> []
	| [x] -> [[x]]
	| x :: xs ->
			let sub_build = build_whole_arnm_internal xs in
			[x] :: (prepend_all x sub_build)

let build_whole_arnm arnms =
	let arnms = build_whole_arnm_internal arnms in
	List.map arnms (fun anm -> StructName(anm))

(* Check that we don't use multiple length variables into the same
   array.  I think that this should probably be made more stringent,
   e.g. to prefer using fewer length variables over more lenght
   variables, and could definitely do with some variable name help.
   *)
let length_variable_compatability (skel: flat_skeleton_binding) =
	let lenvars_for = Hashtbl.create (module String) in
	(* let () = Printf.printf "Staritng new interation\n" in*)
	List.for_all skel.flat_bindings (fun bind ->
		let built_up_arnms =
			build_whole_arnm bind.tovar_index_nesting in
		List.for_all (truncate_zip built_up_arnms bind.valid_dimensions) (fun (arnm, dimvar) ->
			let arnm_so_far_str = (name_reference_to_string arnm) in
			(* let () = Printf.printf "Arnm is %s\n" arnm_so_far_str in *)
			let v_used = Hashtbl.find lenvars_for arnm_so_far_str in
			(* let () = Printf.printf "VUsed is %s\n" (dimvar_mapping_to_string dimvar) in *)
			let _ = Hashtbl.set lenvars_for arnm_so_far_str dimvar in
			match v_used with
			| None ->
					true
			| Some(other) ->
					dimvar_equal dimvar other
		)
	)

let no_multiple_lengths options tbl binding_list =
	let () = if options.debug_skeleton_multiple_lengths_filter then
		let () = Printf.printf "Trying to check if  has multiple lengths: %s\n" (flat_skeleton_type_binding_to_string binding_list) in
		()
	else ()
	in
    let result = List.for_all binding_list.flat_bindings (fun fb ->
		let tname_so_far = ref [] in
		let fname_so_far = ref [] in
        let fromvars = match fb.fromvars_index_nesting with
        | [] -> (* This is just a def --- we still want to consider the tovar
                    but we need to zip right for the next section.  So just spoof it :) *)
        (* This is literally a teribly hack. *)
        (* Should not escape this method at all *)
            [AssignConstant(Int64V(0))]
		| other -> other
        in
		List.for_all fromvars (fun fromvar ->
			let fvar_contents = match fromvar with
			| AssignConstant(_) ->
					[None]
			| AssignVariable(v) ->
					List.map v (fun v -> Some(v))
			in
		List.for_all (truncate_zip (extend_zip fb.tovar_index_nesting fvar_contents) fb.valid_dimensions) (fun ((t, f), dimvar) ->
			let () = tname_so_far := t :: !tname_so_far in
			let has_tbinds = Hashtbl.find tbl (name_reference_list_to_string !tname_so_far) in
			let has_fbinds = match f with
			| Some(v) ->
					let () = fname_so_far := v :: !fname_so_far in
					let result = Hashtbl.find tbl (name_reference_list_to_string !fname_so_far) in
					(* Set the used dimensions for the fvar.  *)
					let _ = Hashtbl.set tbl (name_reference_list_to_string !fname_so_far) dimvar in
					result
			(* If we are assigning a constant, then we don't have to bother. *)
			| None -> None
			in
			let () = if options.debug_skeleton_multiple_lengths_filter then
				let () = Printf.printf "Considering fromvar %s and tovar %s\n" (name_reference_list_to_string !fname_so_far) (name_reference_list_to_string !tname_so_far) in
				let () = Printf.printf "Under dim %s\n" (dimvar_mapping_to_string dimvar) in
				()
			else ()
			in
			let tbinds_valid =
				match has_tbinds with
				| None ->
						true
				| Some(other) ->
						(* We could use a more loose sense of equality
						here, e.g. one that is closer to "could equal",
						but this seems more sensible *)
						let () = if options.debug_skeleton_multiple_lengths_filter then
							let () = Printf.printf "Comparing (tbinds) %s and %s\n" (dimvar_mapping_to_string dimvar) (dimvar_mapping_to_string other) in
							() else ()
						in
						dimvar_equal dimvar other
			in
			let fbinds_valid =
				match has_fbinds with
				| None ->
						true
				| Some(other: dimvar_mapping) ->
						let () = if options.debug_skeleton_multiple_lengths_filter then
							let () = Printf.printf "Comparing %s and %s\n" (dimvar_mapping_to_string dimvar) (dimvar_mapping_to_string other) in
							() else ()
						in
						dimvar_equal dimvar other
			in
			(* Now, set the used dimensions for the tvar *)
			let _ = Hashtbl.set tbl (name_reference_list_to_string !tname_so_far) dimvar in
			tbinds_valid && fbinds_valid
		)
		)
	) in
	let () =
		if options.debug_skeleton_multiple_lengths_filter then
			let () = Printf.printf "Keep that skeleton bool is %b\n" (result)
			in ()
		else ()
    in
    result

let no_multiple_lengths_check options ((range, pre), post) =
	let lenvar_ass = Hashtbl.create (module String) in
	let pre_asses = no_multiple_lengths options lenvar_ass pre in
	let post_asses = no_multiple_lengths options lenvar_ass post in
	pre_asses && post_asses

(* Check a single skeleton.  *)
let skeleton_check skel =
	(* Don't assign to multiple interface variables
	from the same input variable --- that seems
	unlikely to happen in most contexts.  *)
	no_multiplie_cloning_check skel

let skeleton_pair_check options p =
	(* Don't assign to/from a variable using different
	length parameters.  *)
	no_multiple_lengths_check options p
