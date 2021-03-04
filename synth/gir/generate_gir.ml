open Core_kernel;;
open Options;;
open Spec_definition;;
open Spec_utils;;
open Skeleton;;
open Skeleton_definition;;
open Skeleton_utils;;
open Gir;;
open Gir_clean;;
open Gir_utils;;
open Gir_topology;;
open Utils;;
open Builtin_conversion_functions;;

exception GenerateGIRException of string

let induction_variable_count = ref 0
let new_induction_variable () =
    let () = induction_variable_count := !induction_variable_count + 1 in
    Name("i" ^ (string_of_int !induction_variable_count))

let variable_count = ref 0
let new_variable () =
    let () = variable_count := !variable_count + 1 in
    Name("v_" ^ (string_of_int !variable_count))

let conversion_function_count = ref 0
let new_conversion_function () =
    let () = conversion_function_count := !conversion_function_count + 1 in
    Name("conversion" ^ (string_of_int !conversion_function_count))

let rec generate_variable_reference_to namerefs =
    match namerefs with
    | AnonymousName -> raise (GenerateGIRException "Can't genereate a variable reference from anonymous names!")
    (* Name mappings are easy. *)
    | Name(n) -> Variable(Name(n))
    | StructName(nlist) ->
            match nlist with
            | [] -> raise (GenerateGIRException "Can't generate variable references from empty names!")
            | [Name(x)] -> Variable(Name(x))
            | [x] -> raise (GenerateGIRException "Pretty sure this sin't possible")
            | xs ->
                    (* This could probably be more efficient. *)
                    let xs = List.rev xs in
                    let head_xs, tail_xs = List.hd_exn xs, List.tl_exn xs in
                    let head_name = (match head_xs with
                    | Name(x) -> x
                    | _ -> raise (GenerateGIRException "pretty sure this isn't possible")
                    ) in
                    let original_tail = List.rev tail_xs in
                    MemberReference(generate_variable_reference_to (StructName(original_tail)), Name(head_name))

(*  This should generate a list of functions
that can be used to generate wrappers when
given a simple assignment sequence Assignment.  *)
(* It also keeps track of the index variables *)
let rec generate_loop_wrappers_from_dimensions dim =
	match dim with
	| DimvarOneDimension(dimvar) -> (
			(* Generate a loop for each of the dimvars.  *)
			(* Also try just a straight up assignment.  *)
            match dimvar with
			| ExactVarMatch(from, tov) ->
				let indvar = new_induction_variable () in
                let in_loop_assign =(fun assign ->
                    LoopOver(assign, indvar, generate_variable_reference_to from)
                    ) in
                (in_loop_assign, [indvar])
    )

let rec maybe_create_reference_from post_indexes indvarnames =
	(* let post_indexes_str =
		String.concat ~sep:", " (List.map post_indexes (fun p ->
			match p with
			| None -> "None"
			| Some(p) -> variable_reference_to_string p
		)
		) in
	let indvarnames_str = gir_name_list_to_string indvarnames in
	let () = Printf.printf "Looking at names %s and %s\n" (post_indexes_str) (indvarnames_str) in *)
	match post_indexes, indvarnames with
	| [], [] -> raise (GenerateGIRException "Must be non empty!")
	| p :: [], [] -> p
	| p :: ps, i :: is ->
			let subref = maybe_create_reference_from ps is in (
			match p, subref with
			| None, None ->
					None
			| None, Some(subref) ->
					Some(IndexReference(subref, VariableReference(Variable(i))))
			| Some(p), None ->
					Some(IndexReference(
						p, VariableReference(Variable(i))
					))
			| Some(p), Some(subref) ->
					Some(
						build_reference_chain
							(IndexReference(
								subref,
								VariableReference(Variable(i))
							))
							p
					)
			)
	| _ -> raise (GenerateGIRException "Indexes and indvarnames must be the same length!")

let create_reference_from post_indexes indvarnames =
	(* This thing only works with the list backwards apparently.  *)
	let result = maybe_create_reference_from (List.rev post_indexes) indvarnames in
	match result with
	| None -> raise (GenerateGIRException "Ended up producing no result!")
	| Some(x) -> x

(* This generates a set of functions that take
a list representing the variables that are used to index
this.  *)
let generate_assign_functions conversion_function_name fvar_index_nestings tvar_index_nesting =
	if (List.length fvar_index_nestings) = 0 then
		[]
	else
		List.map fvar_index_nestings (fun fvar_ind_nest -> (
				fun index_vars ->
					(* We expect one index_var for each fromvar_index and each tovar_index --- those
					capture the parts of the variable names
					that are refered to by each.  *)
					(* let () = Printf.printf "Ind nest is %s\n" (variable_reference_option_list_to_string fvar_ind_nest) in
					let () = Printf.printf "Index vars is %s\n" (gir_name_list_to_string index_vars) in
					let () = Printf.printf "tvars is %s\n" (variable_reference_option_list_to_string tvar_index_nesting) in
					let () = Printf.printf "fvar ind nest length is %d \n " (List.length fvar_index_nestings) in *)
					let () = assert(List.length(fvar_ind_nest) - 1 = List.length index_vars) in
					let () = assert(List.length(tvar_index_nesting) - 1 = List.length index_vars) in
					(* Get the LVars --- if there
					are no indexes then it is just
					a list, otherwise we need to do 
					a fold. *)
					let lvars = LVariable(create_reference_from tvar_index_nesting index_vars) in
					let rvars: rvalue = Expression(VariableReference(create_reference_from fvar_ind_nest index_vars)) in
					Assignment(lvars, rvars)
			)
		)

let get_define_for vnameref =
    match vnameref with
    | Variable(nam) ->
        Definition(nam)
    (* It's hard to say for sure what to do here --- eg.
    if you have
    struct {
        int *args;
    } mystruct;
    you'd want to eb able to define args to point e.g. to
    the stack.  But perhaps it's just better to define
    that at the same point you define the mystruct?
    Not really sure.  *)
    | _ -> raise (GenerateGIRException "Don't know how to define a more complicated type right now!")

(* TODO -- really need to fix this crappy conversion
from the name_references to gir names --- there's way
too many exceptions flying around in all these implementations
*)
let generate_gir_name_for nref =
    match nref with
    | AnonymousName -> None
    | Name(n) -> Some(Variable(Name(n)))
    | StructName(names) ->
            let names = List.map names (fun n -> match n with
            | Name(n) -> n
            | _ -> raise (GenerateGIRException "UNexepcted!")
            ) in
            let n, ns = match names with
            | n :: ns -> n, ns
            | _ -> raise (GenerateGIRException "Empyt struct name!") in
            Some(List.fold
                ns
                ~init:(Variable(Name(n)))
                ~f:(fun namesofar nextname ->
                    MemberReference(namesofar, Name(nextname))))

(* takes some variable that's been split up into e.g.
cpx ['x', 'y'] where it's intended use is
cpx.x[i].y[j] and turns it into cpx.x.y.  It's used
in tandem with the above function, not sure whether
it's actually going to be useful for e.g. multiple
dimensions or not.  *)
let rec define_name_of index_points =
	match index_points with
	| [] -> raise (GenerateGIRException "Can't define empty varaiable")
	(* Do the define before the first index? idk *)
	| Some(x) :: xs -> x
	(* Pretty sure it should always hit ^^^ *)
	| None :: xs -> define_name_of xs

let generate_gir_names_for nrefs =
    List.map nrefs generate_gir_name_for

let generate_unwrapped_gir_name_for (nref: name_reference): gir_name =
    match nref with
    | Name(n) -> Name(n)
    | _ -> raise (GenerateGIRException "Can't convert anything that isn't a name!")

let generate_unwrapped_gir_names_for nrefs =
    List.map nrefs generate_unwrapped_gir_name_for

let rec get_bindings_by_name tvars dims =
	(* let () = Printf.printf "Getting bindings for %s under dims %s\n" (name_reference_list_to_string tvars) (dimvar_mapping_list_to_string dims) in *)
	let result = match tvars, dims with
    | _, [] -> []
    | t :: tvars, d :: dims ->
            (* We pick the 't' variable, because that should be live
               coming into the function so should just be a bit less
               complicated.  I think I used the 'f' variable before,
               and that worked fine too.  May need to re-evaluate as
               things change. *)
            let sdim_domain_var = match d with
            (*  Needs to only havea s inle entry -- keeping it like this
            because I think we may want more complex types in the
            future here.  *)
            | DimvarOneDimension(ExactVarMatch(f, t)) -> Dimension([f])
            in
            let subdims = get_bindings_by_name tvars dims in
            (* Need to put the tvar name on the front of all
               those. *)
            let prepended_subsims = List.map subdims (fun (sname, sdim) ->
                match sname with
                | StructName(ns) -> (StructName(t :: ns), sdim)
                | Name(_) -> (StructName([t; sname]), sdim)
                | AnonymousName -> (t, sdim)
            ) in
            (t, sdim_domain_var) :: prepended_subsims
    | [], _ :: _ -> raise (GenerateGIRException "Can't have fewer dims than var splits\n") in
	(* let () = Printf.printf "Result is %s\n" (String.concat ~sep:", " (List.map result (fun (f, t) ->
		(name_reference_to_string f) ^ " -> " ^ (dimension_type_to_string t)
	))) in *)
	result

let merge_bindings_by_name binds1 binds2 =
	(* let () = Printf.printf "B1 is is %s\n" (String.concat ~sep:", " (List.map binds1 (fun (f, t) ->
		(name_reference_to_string f) ^ " -> " ^ (dimension_type_to_string t)
	))) in
	let () = Printf.printf "B2 is is %s\n" (String.concat ~sep:", " (List.map binds2 (fun (f, t) ->
		(name_reference_to_string f) ^ " -> " ^ (dimension_type_to_string t)
	))) in *)
	let result = remove_duplicates (fun (n1, dv1) -> fun (n2, dv2) ->
        let eq = name_reference_equal n1 n2 in
        let () = if eq then assert (dimension_type_equal dv1 dv2) else () in
        eq
    ) (binds1 @ binds2) in
	(* let () = Printf.printf "result is %s\n" (String.concat ~sep:", " (List.map result (fun (f, t) ->
		(name_reference_to_string f) ^ " -> " ^ (dimension_type_to_string t)
	))) in *)
	result

let binding_lists_to_string bs =
    List.map bs (fun (from, tom) ->
        (name_reference_to_string from, tom)
    )

let generate_conversion_function conv = match conv with
    | IdentityConversion ->
            (* We don't need to do anything here --- this
            should be eliminated by further passes.
            *)
            EmptyGIR, Name("identity")
    | Map(ftype, ttype, to_from_list) ->
			let to_from_list_synths = List.map to_from_list (fun (tov, fromv) ->
				(synth_value_from_range_value tov, 
				 synth_value_from_range_value fromv)
			) in
            let fname = new_conversion_function () in
            let argname = new_variable () in
            let returnvar = new_variable () in
            (* Create the typelookup.  *)
            let typelookup = Hashtbl.create (module String) in
            (* And add the functions to it.  *)
            let _ = Hashtbl.add typelookup (gir_name_to_string argname) ftype in
            let _ = Hashtbl.add typelookup (gir_name_to_string returnvar) ttype in
            let _ = Hashtbl.add typelookup (gir_name_to_string fname) (Fun(ftype, ttype)) in
            FunctionDef(fname,
                VariableList([
                    Variable(argname);
                ]
                ),
                Sequence(
                [Assignment(
                    LVariable(Variable(returnvar)),
                    Expression(GIRMap(Variable(argname), to_from_list_synths))
                );
                Return(returnvar)
                ]
                ),
                typelookup
            ), fname

let generate_gir_for_binding define_before_assign (options: options) (skeleton: flat_skeleton_binding) =
	(* First, compute the expression options for each
	   binding, e.g. it may be that we could do
	   x = cos(y) or x = sin(y) or x = y. 
	   Note that I think this should no longer happen
	   here, but I'm going to leave that comment anyway. *)
	let expression_options, required_fun_defs = List.unzip (List.map skeleton.flat_bindings (fun (single_variable_binding: flat_single_variable_binding) ->
		(* There may be more than one valid dimension value.
		   generate assignments based on all the dimension values. *)
        (* TODO --- fix this shit -- I'm pretty sure
        the loop gen is broken for 2D loops.  *)
		let () = if options.debug_generate_gir then
			let () = Printf.printf "Starting new binding gen for binding\n" in
			let () = Printf.printf "%s\n" (flat_single_variable_binding_to_string single_variable_binding) in
            ()
		else () in
		let loop_wrappers = List.map single_variable_binding.valid_dimensions
			generate_loop_wrappers_from_dimensions in
		let conversion_function, conversion_function_name = generate_conversion_function single_variable_binding.conversion_function in
        let fvars_indexes = List.map single_variable_binding.fromvars_index_nesting generate_gir_names_for in
        let tovar_indexes = generate_gir_names_for single_variable_binding.tovar_index_nesting in
        (* Convert the variable references mentioned
            in the bindings into real variable refs.  *)
		(* Generate the possible assignments *)
		let assign_funcs = generate_assign_functions conversion_function_name fvars_indexes tovar_indexes in
		(* Get the define if required.  *)
		let define = if define_before_assign then
			let () = if options.debug_generate_gir then
				let () = Printf.printf "Have the following tovars for the generation round:\n " in
				let () = Printf.printf "%s\n" (name_reference_list_to_string single_variable_binding.tovar_index_nesting) in
			() else () in
			get_define_for (define_name_of tovar_indexes)
		else
			EmptyGIR in
        let () =
            if options.debug_generate_gir then
                let () = Printf.printf "------\n\nFor flat skeleton %s\n" (flat_skeleton_list_to_string [skeleton]) in
				let () = Printf.printf "Valid dimensions were %s\n" (dimvar_mapping_list_to_string single_variable_binding.valid_dimensions) in
                let () = Printf.printf "Loop wrappers found are %d\n" (List.length loop_wrappers) in
				let () = Printf.printf "Loop assignment functions are %d\n" (List.length assign_funcs) in
				Printf.printf "Define used is %s\n" (gir_to_string define)
            else
                () in
		(* Do every combination of assignment loops and assign funcs. *)
		let assignment_statements =
			if (List.length loop_wrappers > 0) then
				List.concat (List.map loop_wrappers (fun (lwrap, ind_vars) ->
					List.map assign_funcs (fun assfunc ->
						(* Combine the loops! *)
						lwrap (assfunc ind_vars)
					)
				))
			else
				(* If there are no loops, we can just do the raw assignments.  *)
                (List.map assign_funcs (fun assfunc -> assfunc []))
		in
		let assigns_with_defines =
			if define_before_assign then
				if (List.length assignment_statements) > 0 then
					List.map assignment_statements (fun ass -> Sequence([define; ass]))
				else
					(* Some vars can be define-only *)
					[define]
			else
				assignment_statements in
		(* Also return the conversion functions needed for this
			assign. *)
		assigns_with_defines, conversion_function
	)
	)in
    (* Get the length variable bindings *)
    let len_bindings = List.concat (List.map skeleton.flat_bindings (fun (single_variable_binding: flat_single_variable_binding) ->
        get_bindings_by_name single_variable_binding.tovar_index_nesting single_variable_binding.valid_dimensions
    )) in
	(* We now have a expression list list, where we need one element
	   from each sublist in sequence to form complete assignment
	   tree.  *)
	let () = if options.debug_generate_gir then
		let () = Printf.printf "Len bindings are %s\n" (String.concat ~sep:", " (List.map len_bindings (fun (f, t) ->
			(name_reference_to_string f) ^ " -> " ^ (dimension_type_to_string t)
		))) in
		let () = Printf.printf "Have the following expression options before cross product: %s\n"
			(gir_list_list_to_string expression_options) in
		let () = Printf.printf "This amounts to %d lists\n" (List.length expression_options) in
		()
	else () in
	let expr_lists: gir list list = cross_product expression_options in
	(* Now we have a expression list list where each set
	   is a full set of assignments.  Convert each expr list
	   to a sequence.  *)
	let code_options = List.map expr_lists (fun exprs -> Sequence(exprs)) in
	(* Do a quick cleanup --- e.g. making sure that there are no double
	defines, which this approach is prone to generating.  *)
	let cleaned_code_options = List.map code_options gir_double_define_clean in
	cleaned_code_options, len_bindings, required_fun_defs

let rec all_dimvars_from dimtype =
	match dimtype with
			| EmptyDimension -> []
			| Dimension(nms) -> generate_unwrapped_gir_names_for nms

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
	| Name(nm), (Name(y) :: ys) -> ((String.compare nm y) = 0) || (member x ys)
	(* | _ -> raise (GenerateGIRException "Unimplemented") *)

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

let generate_gir_for options (api: apispec) ((pre_skeleton: flat_skeleton_binding), (post_skeleton: flat_skeleton_binding)) =
	let () = if options.debug_generate_gir then
		Printf.printf "Starting generation for new skeleton pair\n"
	else () in
    (* Get the define statements required for the API inputs.  *)
	(* Define the variables before assign in the pre-skeleton case.  *)
	let pre_gir, pre_lenbinds, pre_required_fun_defs = generate_gir_for_binding true options pre_skeleton in
	let post_gir, post_lenbinds, post_required_fun_defs = generate_gir_for_binding false options post_skeleton in
    (* Keep track of the variable length assignments that have been made. *)
    let merged_lenbinds = binding_lists_to_string (merge_bindings_by_name pre_lenbinds post_lenbinds) in
    let table_lenbinds = hash_table_from_list (module String) merged_lenbinds in
	let all_fundefs = pre_required_fun_defs @ post_required_fun_defs in
	let res = List.cartesian_product pre_gir post_gir in
	let () = if options.debug_generate_gir then
		let () = Printf.printf "Finished generation of candidata pre programs.  Program are:\n%s\n"
			(String.concat ~sep:"\n\n" (List.map pre_gir gir_to_string)) in
		let () = Printf.printf "Finsihed generation of candiates post programs. Programs are:\n%s\n"
			(String.concat ~sep:"\n\n" (List.map post_gir gir_to_string)) in
		Printf.printf "Found %d pre and %d post elements\n" (List.length pre_gir) (List.length post_gir)
	else () in
    List.map res (fun (pre, post) -> (pre, post, table_lenbinds, all_fundefs))


let generate_gir (options:options) classmap iospec api skeletons: ((gir_pair) list) =
	let result = List.concat ((List.map skeletons (fun skel ->
		generate_gir_for options api skel))) in
	let () = if options.dump_generate_gir then
		let () = Printf.printf "Generated %d GIR-pair programs\n" (List.length result) in
		Printf.printf "Printing these programs below:\n%s\n" (String.concat ~sep:"\n\n\n" (List.map result (fun(pre, post, binds, funs) ->
			"Pre:" ^ (gir_to_string pre) ^ "\nPost: " ^ (gir_to_string post))))
	else () in
	List.map result (fun (pre, post, bindings, fundefs) ->
		{
			pre = pre;
            post = post;
			lenvar_bindings = bindings;
			fundefs = fundefs;
		})
