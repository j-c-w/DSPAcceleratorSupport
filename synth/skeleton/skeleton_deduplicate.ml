open Core_kernel;;
open Skeleton_definition;;
open Skeleton_utils;;
open Spec_utils;;
open Spec_definition;;
open Builtin_conversion_functions;;

(* This should really implement a 'proper' hashing of
	the skeleton programs.

	The aim is to remove 'effective' duplicates,
	that are introduced due to different
	infered typemaps.  *)

(* So this part in particular is a horrendous hack! *)
(* There is definitely an easy program-numbering
way to do it. *)
let flat_skeleton_binding_to_hash_string tmap binding =
	String.concat ~sep:"-" (
		List.map binding.flat_bindings (fun binds ->
				let totyp = synth_type_to_string (type_of_name_reference_list tmap (binds.tovar_index_nesting)) in
				let fromtyps = String.concat ~sep:", " (List.map binds.fromvars_index_nesting (fun v -> (synth_type_to_string (type_of_assignment tmap v)))) in
				"To:" ^ (name_reference_list_to_string binds.tovar_index_nesting) ^ ", Typ: " ^ totyp ^ ", From:" ^ (assignment_type_list_to_string binds.fromvars_index_nesting) ^ ", FromTyp:" ^ fromtyps ^ ", Dimensions:" ^ (dimvar_mapping_list_to_string binds.valid_dimensions) ^ ", ConvFunc:" ^ (conversion_function_to_string binds.conversion_function)
		)
	)

let skeleton_pairs_to_hash_string (s: skeleton_pairs) =
	(* We obviously don't wnat to consider the typemap,
	since this is meant to consider invariancies
	under that.  I /think/ that there should
	only be one rangecheck per candidate anyway.
	Famous last words I suppose, can imagine
	this being nasty to debug. *)
	(flat_skeleton_binding_to_hash_string s.typemap s.pre) ^
	(flat_skeleton_binding_to_hash_string s.typemap s.post)

let skeleton_to_hash s =
	(skeleton_pairs_to_hash_string s)

let deduplicate_skeletons options skeletons =
	let hashes = List.map skeletons (fun s ->
		skeleton_to_hash s, s
	) in
	let hashtable = Hashtbl.create (module String) in
	let filtered = List.filter hashes (fun (hash, prog) ->
		match Hashtbl.add hashtable hash true with
		| `Ok -> true
		| `Duplicate -> false
	) in
	List.map filtered (fun (_, p) -> p)
