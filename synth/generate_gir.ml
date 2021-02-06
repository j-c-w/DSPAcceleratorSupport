open Core_kernel;;
open Options;;
open Spec_definition;;
open Spec_utils;;
open Skeleton;;
open Gir;;
open Gir_utils;;


let rec cross_product ls =
	match ls with
	| [] -> []
	| [x] -> List.map x (fun value -> [value])
	| (x :: xs) ->
			let subcross = cross_product xs in
			List.concat(
			List.map subcross (fun subprod ->
				List.map x (fun value -> 
					value :: subprod
				)
			)
			)

(*  This should generate a list of functions
that can be used to generate wrappers when
given a simple assignment sequence Assignment.  *)
let rec generate_loop_wrappers_from_dimensions dim =
	match dim with
	| DimvarOneDimension(dimvar) -> (
			(* Generate a loop for each of the dimvars.  *)
			(* Also try just a straight up assignment.  *)
            match dimvar with
            | ExactVarMatch(from, tov) ->
                [(fun assign -> assign);
                (fun assign ->
                    LoopOver(assign, from)
                )]
    )
	| DimvarHigherDimension(subdim, dim_mapping) ->
			(* Compute the loops for the subdimensions,
			   then add this loop on top.  *)
			let subloops = generate_loop_wrappers_from_dimensions subdim in
            List.map subloops (fun sloop ->
                match dim_mapping with
                | ExactVarMatch(from, tov) ->
                    fun assign ->
                        LoopOver((sloop assign), from)
			)

let generate_assign_functions fvars tvar =
	match fvars, tvar with
	| [], _ -> [] (* TODO -- maybe some constant gen here?  Need it somewhere. *)
	(* TODO --- need to be able to generate more complex
       conversion functions here.  *)
	| fvars, _ -> List.map fvars (fun fvar ->
			Assignment(LVariable(tvar), RReference(fvar)))


let generate_gir_for_binding (options: options) skeleton: gir list =
	(* First, compute the expression options for each
	   binding, e.g. it may be that we could do
	   x = cos(y) or x = sin(y) or x = y.  *)
	let expression_options = List.map skeleton.bindings (fun single_variable_binding ->
		(* There may be more than one valid dimension value.
		   generate assignments based on all the dimension values. *)
		let loop_wrappers = List.concat (List.map single_variable_binding.valid_dimensions 
            generate_loop_wrappers_from_dimensions) in
		(* Generate the possible assignments *)
		let assign_funcs = generate_assign_functions single_variable_binding.fromvars single_variable_binding.tovar in
		(* Do every combination of assignment loops and assign funcs. *)
		let assignment_statements =
			List.concat (List.map loop_wrappers (fun lwrap ->
				List.map assign_funcs (fun assfunc ->
					(* Combine the loops! *)
					lwrap assfunc
				)
			))
		in
		assignment_statements
	) in
	(* We now have a expression list list, where we need one element
	   from each sublist in sequence to form complete assignment
	   tree.  *)
	let () = if options.debug_generate_gir then
		Printf.printf "Have the following expression options before cross product: %s\n"
			(gir_list_list_to_string expression_options)
	else () in
	let expr_lists: gir list list = cross_product expression_options in
	(* Now we have a expression list list where each set
	   is a full set of assignments.  Convert each expr list
	   to a sequence.  *)
	List.map expr_lists (fun exprs -> Sequence(exprs))

let generate_gir_for options ((pre_skeleton: skeleton_type_binding), (post_skeleton: skeleton_type_binding)) =
	let () = if options.debug_generate_gir then
		Printf.printf "Starting generation for new skeleton pair\n"
	else () in
	let pre_gir = generate_gir_for_binding options pre_skeleton in
	let post_gir = generate_gir_for_binding options post_skeleton in
	let res = List.cartesian_product pre_gir post_gir in
	let () = if options.debug_generate_gir then
		let () = Printf.printf "Finished generation of candidata pre programs.  Program are:\n%s\n"
			(String.concat ~sep:"\n\n" (List.map pre_gir gir_to_string)) in
		Printf.printf "Finsihed generation of candiates post programs. Programs are:\n%s\n"
			(String.concat ~sep:"\n\n" (List.map post_gir gir_to_string))
	else () in
	res


let generate_gir (options:options) classmap iospec api skeletons: ((gir * gir) list) =
	let result = List.concat ((List.map skeletons (fun skel ->
		generate_gir_for options skel))) in
	let () = if options.dump_generate_gir then
		let () = Printf.printf "Generated %d GIR-pair programs\n" (List.length result) in
		Printf.printf "Printing these programs below:\n%s\n" (String.concat ~sep:"\n\n\n" (List.map result (fun(pre, post) ->
			"Pre:" ^ (gir_to_string pre) ^ "\nPost: " ^ (gir_to_string post))))
	else () in
	result
