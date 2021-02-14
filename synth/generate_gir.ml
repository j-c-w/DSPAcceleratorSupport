open Core_kernel;;
open Options;;
open Spec_definition;;
open Spec_utils;;
open Skeleton;;
open Gir;;
open Gir_utils;;
open Gir_topology;;

exception GenerateGIRException of string

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

let induction_variable_count = ref 0
let new_induction_variable () =
    let () = induction_variable_count := !induction_variable_count + 1 in
    Name("i" ^ (string_of_int !induction_variable_count))

(* This takes some expression, and turns it into
   an equivalent expression indexed by some index
   variable.  *)
let rec add_dimension_to gir (index_variable: name_reference) =
	match gir with
	| Assignment(lval, Expression(rval)) ->
			(* TODO -- this might need to become more
			complicated to support more complicated assigns. *)
			Assignment(LIndex(lval, VariableReference(index_variable)), Expression(ListIndex(rval, VariableReference(index_variable))))
	| _ ->
			raise (GenerateGIRException "More implementation work needed to support this.")

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
					let indvar = new_induction_variable () in
                    LoopOver(add_dimension_to assign indvar, indvar, from)
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
						let indvar = new_induction_variable () in
                        LoopOver(sloop (add_dimension_to assign indvar), indvar, from)
			)

let generate_assign_functions fvars tvar =
	match fvars, tvar with
	| [], _ -> [] (* TODO -- maybe some constant gen here?  Need it somewhere. *)
	(* TODO --- need to be able to generate more complex
       conversion functions here.  *)
	| fvars, _ -> List.map fvars (fun fvar ->
			Assignment(LVariable(tvar), Expression(VariableReference(fvar))))

let get_define_for vnameref =
	Definition(vnameref)

let generate_gir_for_binding define_before_assign (options: options) skeleton: gir list =
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
		(* Get the define if required.  *)
		let define = if define_before_assign then
			get_define_for single_variable_binding.tovar
		else
			EmptyGIR in
        let () =
            if options.debug_generate_gir then
                let () = Printf.printf "------\n\nFor skeleton %s\n" (skeleton_list_to_string [skeleton]) in
				let () = Printf.printf "Valid dimensions were %s\n" (dimvar_mapping_list_to_string single_variable_binding.valid_dimensions) in
                let () = Printf.printf "Loop wrappers found are %d\n" (List.length loop_wrappers) in
                Printf.printf "Loop assignment functions are %d\n" (List.length assign_funcs)
            else
                () in
		(* Do every combination of assignment loops and assign funcs. *)
		let assignment_statements =
			if (List.length loop_wrappers > 0) then
				List.concat (List.map loop_wrappers (fun lwrap ->
					List.map assign_funcs (fun assfunc ->
						(* Combine the loops! *)
						lwrap assfunc
					)
				))
			else
				(* If there are no loops, we can just do the raw assignments.  *)
				assign_funcs
		in
		let assigns_with_defines =
			if define_before_assign then
				List.map assignment_statements (fun ass -> Sequence([define; ass]))
			else
				assignment_statements in
		assigns_with_defines
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
	let code_options = List.map expr_lists (fun exprs -> Sequence(exprs)) in
    code_options

let rec all_dimvars_from dimtype =
	match dimtype with
			| EmptyDimension -> []
			| Dimension(nms) -> nms
			| HigherDimention(subdims, nms) -> nms @ (all_dimvars_from subdims)

let rec type_topo_dependencies (nam, typ) =
	match typ with
	| Array(subtyp, dimtype) ->
			let _, subdeps = type_topo_dependencies (nam, subtyp) in
			(* let () = Printf.printf "For name %s have deps %s \n " (name_reference_to_string nam) (String.concat (List.map (all_dimvars_from dimtype)name_reference_to_string )) in *)
			(nam, (all_dimvars_from dimtype) @ (subdeps))
	(* All non-array types are not dependent types
		in the languages we are currently supporting.  *)
	| _ -> (nam, [])

(* Do a topological sort on a list of (name, depname) dependencies *)
(* This is a horribly inefficient toposort, but I expect
the input problem size to be quite small so shouldn't be an
issue.  *)
let rec member x ys =
	match x, ys with
	| _, [] -> false
	| Name(nm), (Name(y) :: ys) -> (nm = y) || (member x ys)
	| _ -> raise (GenerateGIRException "Unimplemented")

let rec toposort_search_for_deps name possdeps =
	match possdeps with
	| [] -> [], []
	| (nm, deps) :: rest -> 
			let subdeps, nondeps = toposort_search_for_deps name rest in
			if member name deps then
				((nm, deps) :: subdeps), nondeps
			else
				subdeps, ((nm, deps) :: nondeps)

let rec toposort names =
	match names with
	| [] -> []
	| ((nm, deps)) :: rest ->
			let afters, befores = toposort_search_for_deps nm rest in
			(toposort befores) @ (nm :: (toposort afters))


let generate_define_statemens_for options api =
	(* Need to make sure that types that are dependent on each
	   other are presented in the right order.  *)
	let names = List.map api.livein (fun n -> (Name(n), Hashtbl.find_exn api.typemap n)) in
	let sorted_names = toposort (List.map names type_topo_dependencies) in
	(* let () = Printf.printf "Names %s\n" (String.concat((List.map names (fun (n, s) -> (match n with Name(x) -> x) ^ (synth_type_to_string s))))) in *)
	(* let () = Printf.printf "Sorte names %s\n" (String.concat ~sep:", " (List.map sorted_names name_reference_to_string)) in *)
    (* Generate a define for each input variable in the API *)
    List.map sorted_names (fun x -> Definition(x))

let generate_gir_for options (api: apispec) ((pre_skeleton: skeleton_type_binding), (post_skeleton: skeleton_type_binding)) =
	let () = if options.debug_generate_gir then
		Printf.printf "Starting generation for new skeleton pair\n"
	else () in
    (* Get the define statements required for the API inputs.  *)
	(* Define the variables before assign in the pre-skeleton case.  *)
	let pre_gir = generate_gir_for_binding true options pre_skeleton in
	let post_gir = generate_gir_for_binding false options post_skeleton in
	let res = List.cartesian_product pre_gir post_gir in
	let () = if options.debug_generate_gir then
		let () = Printf.printf "Finished generation of candidata pre programs.  Program are:\n%s\n"
			(String.concat ~sep:"\n\n" (List.map pre_gir gir_to_string)) in
		let () = Printf.printf "Finsihed generation of candiates post programs. Programs are:\n%s\n"
			(String.concat ~sep:"\n\n" (List.map post_gir gir_to_string)) in
		Printf.printf "Found %d pre and %d post elements\n" (List.length pre_gir) (List.length post_gir)
	else () in
    res


let generate_gir (options:options) classmap iospec api skeletons: ((gir * gir) list) =
	let result = List.concat ((List.map skeletons (fun skel ->
		generate_gir_for options api skel))) in
	let () = if options.dump_generate_gir then
		let () = Printf.printf "Generated %d GIR-pair programs\n" (List.length result) in
		Printf.printf "Printing these programs below:\n%s\n" (String.concat ~sep:"\n\n\n" (List.map result (fun(pre, post) ->
			"Pre:" ^ (gir_to_string pre) ^ "\nPost: " ^ (gir_to_string post))))
	else () in
	result
