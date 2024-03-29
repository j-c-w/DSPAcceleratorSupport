open Core;;
open Spec_definition;;
open Spec_utils;;
open Skeleton_definition;;
open Skeleton_utils;;
open Skeleton_filter;;
open Options;;
open Utils;;
open Builtin_conversion_functions;;

let flatten_binding options (svar_binding: single_variable_binding_option_group) =
    (* let () = Printf.printf "Length of bindings is %d\n" (List.length svar_binding.dimensions_set) in *)
	if List.length svar_binding.dimensions_set > 0 then
		(* let () = ignore( List.map svar_binding.dimensions_set ~f:(fun dims -> Printf.printf "Before reducing, have a constraints set of %s\n" (dimension_constraint_list_to_string dims))) in *)
		let reduced_constraint_set = List.map svar_binding.dimensions_set ~f:(filter_constraints_set options) in
		let combinations = cross_product reduced_constraint_set in
        (* let () = Printf.printf "Reduced constraint set to size %d\n" (List.length combinations) in *)
		List.map combinations ~f:(fun dim ->
		(* let () = Printf.printf "Building for dimensions %s" (dimension_constraint_list_to_string dim) in *)
		{
			fromvars_index_nesting = svar_binding.fromvars_index_nesting;
			tovar_index_nesting = svar_binding.tovar_index_nesting;
			dimensions = dim;
			conversion_function = IdentityConversion
		}
		)
	else
        (* let () = Printf.printf ("Dimensions set size was zero") in*)
		(* Not everything /has/ to have a vlid dimension --
			and we still want to keep those.  *)
		[{
			fromvars_index_nesting = svar_binding.fromvars_index_nesting;
			tovar_index_nesting = svar_binding.tovar_index_nesting;
			dimensions = [];
			conversion_function = IdentityConversion
		}]

(* The skeleton pass generates more than one 'possible binding' per list
   item.   This pass flattens that out into a single list.  *)
let flatten_skeleton (opts: options) (skels: skeleton_type_binding list): flat_skeleton_binding list =
	let () = if opts.debug_skeleton_flatten then
		Printf.printf "Input length is %d\n" (List.length skels)
	else () in
	let vbinds = List.concat (List.map skels ~f:(
			fun skel ->
				let () = if opts.debug_skeleton_flatten then
					let () = Printf.printf "Starting new skeleton!\n" in
					let () = Printf.printf "Scanning length of %d\n" (List.length skel.bindings) in
					let () = Printf.printf "Skeleton is %s\n" (skeleton_type_binding_to_string skel) in
                    let () = Printf.printf "Number of input variables %d\n" (List.length skel.bindings) in
                    let () = Printf.printf "Number of elements per variable is: %s\n"
                                (String.concat ~sep:", " (List.map skel.bindings ~f:(fun b -> Int.to_string (List.length (b.fromvars_index_nesting))))) in
					()
				else () in
                (* Each elt in the skel bindings is a list of possible dimvars *)
				let binding_options = List.map skel.bindings ~f:(flatten_binding opts) in
                (* Product them all together so each is a sinlge list of options.  *)
				let bindings = cross_product binding_options in
                let () = if opts.debug_skeleton_flatten then
                    let () = Printf.printf "Post flattening are %s\n" (
                        flat_single_variable_binding_list_list_to_string bindings
                    ) in
                    let () = Printf.printf "Number of variables is %d\n" (List.length bindings) in
                    () else () in
				bindings
		)
	) in
	let result = List.map vbinds ~f:(fun bind ->
	{
		flat_bindings = bind
	}
    ) in
	let () = if opts.debug_skeleton_flatten then
		Printf.printf "Produced results of length %d (input length %d)\n " (List.length result) (List.length skels)
	else () in
	(* There must be more things after we expand them. *)
    (* Failure of this assertion implies that there is something odd going on
    with the dimension variables.  *)
	(* Note: Oct 2022: with more agressive length inference passes,
	   things sometimes get to this stage without having any pluasible
	   matches.  It's not clear to me whether this assert is important
	   enough to figure out how to abort out of this pipeline before
	   we get here if there are zero matches.

	   The main problem here is that this pass is called for each infered
		typemap --- so if one typemap has no valid mappings for a variable,
		this assert is tripped.

	   Regardless, in less-agressive length inference modes, this isn't hit.
	   Presumably becuase there are partially valid candidates that make it
	   past here.  *)
	(* Anyway, disabled for now.  *)
	(* let () = assert ((List.length result) >= (List.length skels)) in *)
	let result = List.filter result ~f:length_variable_compatability in
	result
