open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Skeleton_definition;;
open Options;;

let rec cartesian_products xs = match xs with
    | [] -> [[]]
    | x :: xs ->
            let subproduct = cartesian_products xs in
            List.map subproduct (fun i ->
                List.concat (
                    List.map x (fun j ->
                        j :: i
                    )
                )
            )

let flatten_binding (svar_binding: single_variable_binding_option_group)  = List.map svar_binding.valid_dimensions_set (fun dim ->
	{
		fromvars_index_nesting = svar_binding.fromvars_index_nesting;
		tovar_index_nesting = svar_binding.tovar_index_nesting;
		valid_dimensions = dim
	}
)


(* The skeleton pass generates more than one 'possible binding' per list
   item.   This pass flattens that out into a single list.  *)
let flatten_skeleton (opts: options) (skels: skeleton_type_binding list): flat_skeleton_binding list =
	let vbinds = List.concat (List.map skels (
			fun skel ->
                (* Each elt in the skel bindings is a list of possible dimvars *)
				let binding_options = List.map skel.bindings flatten_binding in
                (* Product them all together so each is a sinlge list of options.  *)
				let bindings = cartesian_products binding_options in
				bindings
		)
	) in
    List.map vbinds (fun bind ->
	{
		flat_bindings = bind
	}
    )
