open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Skeleton_definition;;
open Skeleton_utils;;
open Options;;
open Utils;;

let flatten_binding (svar_binding: single_variable_binding_option_group) =
	if List.length svar_binding.valid_dimensions_set > 0 then
		List.map svar_binding.valid_dimensions_set (fun dim ->
		{
			fromvars_index_nesting = svar_binding.fromvars_index_nesting;
			tovar_index_nesting = svar_binding.tovar_index_nesting;
			valid_dimensions = dim
		}
		)
	else
		(* Not everything /has/ to have a vlid dimension --
			and we still want to keep those.  *)
		[{
			fromvars_index_nesting = svar_binding.fromvars_index_nesting;
			tovar_index_nesting = svar_binding.tovar_index_nesting;
			valid_dimensions = []
		}]

(* The skeleton pass generates more than one 'possible binding' per list
   item.   This pass flattens that out into a single list.  *)
let flatten_skeleton (opts: options) (skels: skeleton_type_binding list): flat_skeleton_binding list =
	let () = if opts.debug_skeleton_flatten then
		Printf.printf "Input length is %d\n" (List.length skels)
	else () in
	let vbinds = List.concat (List.map skels (
			fun skel ->
				let () = if opts.debug_skeleton_flatten then
					let () = Printf.printf "Starting new skeleton!\n" in
					let () = Printf.printf "Scanning length of %d\n" (List.length skel.bindings) in
					let () = Printf.printf "Skeleton is %s\n" (skeleton_type_binding_to_string skel) in
					()
				else () in
                (* Each elt in the skel bindings is a list of possible dimvars *)
				let binding_options = List.map skel.bindings flatten_binding in
                (* Product them all together so each is a sinlge list of options.  *)
				let bindings = cross_product binding_options in
                let () = if opts.debug_skeleton_flatten then
                    let () = Printf.printf "Post flattening are %s\n" (
                        flat_single_variable_binding_list_list_to_string bindings
                    ) in
                    () else () in
				bindings
		)
	) in
	let result = List.map vbinds (fun bind ->
	{
		flat_bindings = bind
	}
    ) in
	let () = if opts.debug_skeleton_flatten then
		Printf.printf "Produced results of length %d \n " (List.length result)
	else () in
	(* There must be more things after we expand them. *)
	let () = assert ((List.length result) >= (List.length skels)) in
	result
