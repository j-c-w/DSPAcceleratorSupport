open Core_kernel;;
open Assign_dimensions;;
open Spec_definition;;
open Spec_utils;;
open Infer_struct;;

(* Given some fixed input typemap, this function
	expands it out into a number of different typemaps.
	(e.g. assigning dimension variables where appropriate
	etc.)

	It then applies the heuristics previously used
	in skeleton assignment to try and remove unnessecary
	typemaps.

	It also applies structural inference, which infers
	structures over plain arrays.

	Ideally, both these tasks would be applied by some
	external tool with bettern knowledge of the codebase,
	as both could benefit from a fuzzy syntactic analysis,
	(e.g. what range is the array used up to).
	It is currently possible to specify those things as
	inputs and disable the inference part of this pass.
	However, I don't currently have the link-in to
	that technology.
	*)
let generate_unified_typemaps options classmap (iospec: iospec) iospec_typemap (apispec: apispec) apispec_typemap apispec_alignmap =
	let unified_map = merge_maps iospec_typemap apispec_typemap in
	let cloned_classmap = clone_classmap classmap in
	let full_typemap = {
		variable_map = unified_map;
		classmap = cloned_classmap;
		alignment_map = apispec_alignmap;
	} in
	(* Do the dimension assignments.  *)
	let iospec_dimensions = assign_dimensions options full_typemap (iospec.livein @ iospec.liveout @ iospec.returnvar) in
	let apispec_dimensions = List.concat (
		List.map iospec_dimensions (fun iospec_dim -> assign_dimensions options iospec_dim (apispec.livein @ apispec.liveout))
	) in

	(* Do the structure inference *)
	let struct_inferred =
		List.concat
			(List.map apispec_dimensions (fun typemap ->
				(* Only infer types over the iospec --- assume the API designers have been
				kind enough to provide us with some info.  *)
				infer_structure options typemap iospec.funargs
			)
		)
	in

	(* Clear the iospec/apispec/original classmap to avoid any accidental uses.  *)
	let () = clear_map iospec_typemap in
	let () = clear_map apispec_typemap in
	let () = clear_map classmap in

	struct_inferred
