open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Skeleton_definition;;
open Skeleton_utils;;
open Utils;;

let filter_dimvar_set dms = 
    (* We don't need to have the same dimension with
    the same source var for multiple targets.  *)
    let lookup = Hashtbl.create (module String) in
    List.filter dms (fun dm ->
        match dm with
        | DimvarOneDimension(ExactVarMatch(f, t)) -> (
                let f_str = (name_reference_to_string f) in
                let already_mapped = Hashtbl.find lookup f_str in
                let () = Hashtbl.set lookup f_str true in
                match already_mapped with
                | Some(n) -> false
                | None -> true
		)
    )

(* No variables assigned from more than once.  *)
let no_multiplie_cloning_check (skel: skeleton_type_binding) =
    let assigned_from = Hashtbl.create (module String) in
    let () = ignore(
        List.map skel.bindings (fun bind ->
            List.map bind.fromvars_index_nesting (fun indnest ->
                (* Get the indnesst string *)
                let indnest_string = name_reference_to_string (StructName(indnest)) in
                let existing_count = Hashtbl.find assigned_from indnest_string in
                let newvar = match existing_count with
                | None -> 1
                | Some(x) -> x + 1
                in
                Hashtbl.set assigned_from indnest_string newvar
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
	let () = Printf.printf "Staritng new interation\n" in
	List.for_all skel.flat_bindings (fun bind ->
		let built_up_arnms =
			build_whole_arnm bind.tovar_index_nesting in
		List.for_all (truncate_zip built_up_arnms bind.valid_dimensions) (fun (arnm, dimvar) ->
			let arnm_so_far_str = (name_reference_to_string arnm) in
			let () = Printf.printf "Arnm is %s\n" arnm_so_far_str in
			let v_used = Hashtbl.find lenvars_for arnm_so_far_str in
			let _ = Hashtbl.set lenvars_for arnm_so_far_str dimvar in
			match v_used with
			| None ->
					true
			| Some(other) ->
					dimvar_equal dimvar other
		)
	)

(* Check a single skeleton.  *)
let skeleton_check skel =
	(* Don't assign to multiple interface variables
	from the same input variable --- that seems
	unlikely to happen in most contexts.  *)
	no_multiplie_cloning_check skel

(* TODO --- Some filtering here would be a good idea.  *)
let skeleton_pair_check p = true

