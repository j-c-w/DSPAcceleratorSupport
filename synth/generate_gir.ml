open Core_kernel;;
open Options;;
open Spec_definition;;
open Spec_utils;;
open Skeleton;;
open Gir;;


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
	| EmptyDimension ->
			(* Just do a pure (e.g. pointer or raw
			value) assignment *)
			[fun assign -> assign]
	| Dimension(dimvars) ->
			(* Generate a loop for each of the dimvars.  *)
			(* Also try just a straight up assignment.  *)
			(fun assign -> assign) ::
			List.map dimvars (fun dimvar ->
				fun assign ->
					LoopOver(assign, dimvar )
			)
	| HigherDimention(subdim, names) ->
			(* Compute the loops for the subdimensions,
			   then add this loop on top.  *)
			let subloops = generate_loop_wrappers_from_dimensions subdim in
			List.concat (
				List.map subloops (fun sloop ->
					List.map names (fun name ->
						fun assign ->
							LoopOver((sloop assign), name)
					)
				)
			)

let generate_assign_functions fvars tvar =
	match fvars, tvar with
	| [], _ -> [] (* TODO -- maybe some constant gen here?  Need it somewhere. *)
	(* TODO --- need to be able to generate more complex
       conversion functions here.  *)
	| fvars, _ -> List.map fvars (fun fvar ->
			Assignment(LVariable(tvar), RReference(fvar)))


let generate_gir_for_binding skeleton: gir list =
	(* First, compute the expression options for each
	   binding, e.g. it may be that we could do
	   x = cos(y) or x = sin(y) or x = y.  *)
	let expression_options = List.map skeleton.bindings (fun single_variable_binding ->
		(* There may be more than one valid dimension value.
		   generate assignments based on all the dimension values. *)
		let loop_wrappers = generate_loop_wrappers_from_dimensions single_variable_binding.valid_dimensions in
		(* Generate the possible assignments *)
		let assign_funcs = generate_assign_functions single_variable_binding.fromvars single_variable_binding.tovar in
		(* Do every combination of assignment loops and assign funcs. *)
		let assignment_statements =
			List.map loop_wrappers (fun lwrap ->
				List.map assign_funcs (fun assfunc ->
					(* Combine the loops! *)
					lwrap assfunc
				)
			)
		in
		assignment_statements
	) in
	(* We now have a expression list list, where we need one element
	   from each sublist in sequence to form complete assignment
	   tree.  *)
	let expr_lists: gir list list = List.concat (cross_product expression_options) in
	(* Now we have a expression list list where each set
	   is a full set of assignments.  Convert each expr list
	   to a sequence.  *)
	List.map expr_lists (fun exprs -> Sequence(exprs))

let generate_gir_for ((pre_skeleton: skeleton_type_binding), (post_skeleton: skeleton_type_binding)) =
	List.cartesian_product (generate_gir_for_binding pre_skeleton) (generate_gir_for_binding post_skeleton)

let generate_gir (options:options) classmap iospec api skeletons: ((gir * gir) list) =
	List.concat ((List.map skeletons (fun skel ->
		generate_gir_for skel)))
