open Spec_definition;;
open Spec_utils;;
open Core_kernel;;
open Options;;
open Skeleton_flatten;;
open Skeleton_constants;;
open Skeleton_verify;;
open Skeleton_definition;;
open Skeleton_utils;;
open Skeleton_filter;;
open Skeleton_ranger;;
open Skeleton_range_check;;
open Utils;;

(* This module deals with generating synthesis skeletons.
  It uses a number of heuristics to deal with the problems involved
  in doing so.  *)

exception SkeletonGenerationException of string

type binding_mode =
	| PostBinding (* from api variables to user code variables *)
	| PreBinding (* from user code variables to api variables.  *)

(* THis function turns a SType into a list of types
   by flattening them.  (i.e. SType(A,B,C) -> A,B,C*)
let rec flatten_stype_list stype =
	match stype with
	| SType(_) as t -> [t]
	| STypes(typs) -> List.concat (List.map typs flatten_stype_list)
	(* We don't touch the arrays, because those can't
	   come out of their array wrappers.  We do
	   need to make sure that the subtyps are flattened
	   but I've decided that that should happen right
	   before the call to inspect this type.  *)
	| SArray(name, subtyps, lenvar) as t -> [t]

let rec contains x ys =
	match ys with
	| [] -> false
	| y :: ys -> (x = y) || (contains x ys)

let get_plausible_constants_for optsmap name =
	(* let () = Printf.printf "Looking for consts compatible with %s\n" (skeleton_type_to_string name) in *)
    match Hashtbl.find optsmap (name_reference_to_string (name_refs_from_skeleton name)) with
    | Some(opts) ->
            List.map opts (fun opt -> AssignConstant(opt))
    | None -> []

let rec big_intersection lists =
	match lists with
	| [] -> []
	| [] :: ys -> []
	| (x :: xs) :: ys ->
			if (List.for_all ys (contains x)) then
				x :: (big_intersection (xs :: ys))
			else
				big_intersection (xs :: ys)

let variable_in_type var typ =
	match var, typ with
	| n1, SInt(n2) -> (name_reference_equal n1 n2)
	| n1, SBool(n2) -> (name_reference_equal n1 n2)
	| n1, SFloat(n2) -> (name_reference_equal n1 n2)

let rec flatten_stypes sty = 
	List.concat (List.filter_map sty
	(fun ty -> match ty with
	| SType(x) -> Some([x])
	| STypes(x) -> Some(flatten_stypes x)
	| SArray(_, _, _) -> None))

let rec prepend_all prep all =
	match all with
	| [] -> []
	| x :: xs ->
            match x with
            | AssignConstant(c) ->
                    x :: (prepend_all prep xs)
            | AssignVariable(vn) ->
                    AssignVariable(prep :: vn) :: (prepend_all prep xs)

let rec prepend_all_name_refs prep all =
	match all with
	| [] -> []
	| v :: vs -> (
		match v with
        (* Not 100% sure that this decision is the right decision.
        Perhaps we should preserve anonymity? *)
        | AnonymousName -> v
		| Name(_) as other_name -> StructName([prep; other_name])
		| StructName(existing_list) -> StructName(v :: existing_list)
	) :: (prepend_all_name_refs prep vs)

(* In theory, we'd like to support assignments between
   lists of different lengths.  Currently, we just
   assign one variable at a time.  *)
let dimvar_match x y =
	(* x is the accelerator var, y is the input var *)
    let result = match x, y with
	| DimVariable(vname1, DimEqualityRelation), DimVariable(vname2, DimEqualityRelation) ->
            Some(DimvarOneDimension(VarMatch(vname1, vname2, DimEqualityRelation)))
    | DimVariable(vname1, DimPo2Relation), DimVariable(vname2, DimPo2Relation) ->
			let () = Printf.printf "Generating p02 mapping (%s to %s) \n" (name_reference_to_string vname1) (name_reference_to_string vname2) in
            Some(DimvarOneDimension(VarMatch(vname1, vname2, DimEqualityRelation)))
    | DimVariable(vname1, DimEqualityRelation), DimVariable(vname2, DimPo2Relation) ->
            Some(DimvarOneDimension(VarMatch(vname1, vname2, DimPo2Relation)))
            (* TODO -- do we need an inverse po2 relation? *)
    | DimVariable(vname1, _), DimVariable(vname2, _) ->
            (* TODO --- does it do us any good to do a
            conversion here?  not sure.  *)
            None
	| DimConstant(c1), DimConstant(c2) ->
		if c1 = c2 then
			(* TODO --- somehting? *)
			None
		else
			None
	| DimVariable(v1, r1), DimConstant(c2) ->
			Some(DimvarOneDimension(ConstantMatch(c2)))
	| DimConstant(c1), DimVariable(v2, r1) ->
			(* TODO -- should work with the range
			checker to support this case? *)
			Some(DimvarOneDimension(ConstantMatch(c1)))
    in
    result

let rec dimvar_contains direction x y =
	match y with
	| [] -> []
	| y :: ys ->
			let (x', y') =
				(* Flip as appropriate for the pre or post
				binding.  *)
				match direction with
				| PreBinding ->
						x, y
				| PostBinding ->
						y, x
			in
			let assvar = dimvar_match x' y' in
			match assvar with
			| None -> dimvar_contains direction x ys
			| Some(v) -> v :: (dimvar_contains direction x ys)

let rec dim_has_overlap direction x y =
	List.concat (List.map x (fun xel -> dimvar_contains direction xel y ))

let rec dimensions_overlap direction x y =
	match x, y with
	| EmptyDimension, _ -> []
	| _, EmptyDimension -> []
	| Dimension(nrefs), Dimension(nrefs2) -> dim_has_overlap direction [nrefs] [nrefs2]

let add_name_nr name ty =
	match ty with
    (* Not 100% sure that this is the right decision.  May be
    bset to preserve anonymity *)
    | AnonymousName -> Name(name)
    | Name(_) -> StructName([Name(name); ty])
    | StructName(ns) ->
            StructName(Name(name) :: ns)

let rec add_name name ty =
    match ty with
    | SType(SInt(nr)) -> SType(SInt(add_name_nr name nr))
	| SType(SBool(nr)) -> SType(SBool(add_name_nr name nr))
    | SType(SFloat(nr)) -> SType(SFloat(add_name_nr name nr))
    | STypes(subtyps) ->
            STypes(List.map subtyps (add_name name))
    | SArray(aname, subty, dimvar) ->
            (* I think? We don't have to do the dimvar here? *)
            SArray((add_name_nr name aname), subty, dimvar)

let name_from_opt opt =
    match opt with
    | None -> AnonymousName
    | Some(n) -> Name(n)

(* Generates the base typesets for a class. *)
let rec generate_typesets classmap inptype inpname: skeleton_dimension_group_type =
	match inptype with
	| Struct(name) ->
			(
            let (members, structtypemap) = (match Hashtbl.find classmap name with
			| Some(ClassMetadata(mdata)) -> (mdata.members, mdata.typemap)
			| Some(StructMetadata(mdata)) -> (mdata.members, mdata.typemap)
            | None -> raise (SkeletonGenerationException ("Undefined type " ^ name)))
            in
            (* Use the struct typemap to find the types of the members. *)
            let subtypes = List.map members (fun mem -> (Hashtbl.find_exn structtypemap mem, mem)) in
            (* But need to use the classmap to recurse. *)
			let types = List.map subtypes
				(fun (memtyp, memname) -> (generate_typesets classmap memtyp (Some(memname)))) in
			(* Prepend teh class names *)
			let class_ref_types =
				match inpname with
				| None -> types
				| Some(inpname) ->
					List.map types (fun ty -> add_name inpname ty)
			in
			STypes(class_ref_types))
	| Array(subtyp, lenvar) ->
			(* So here's the thing with passing inpname rather than None here.
			   Let's say you've got char x[] --- what has the name?  the array?
			   or the char?  Passing inpname here basically says both
			   get that name.  Not 100% sure it's the right decision.
			   passing None would mean the char is anon and the array
			   get sthe name.  *)
            let subtyps = generate_typesets classmap subtyp None in
			(* This gives the array the name, so 'x' belongs to '[]'.  *)
			SArray(name_from_opt inpname, subtyps, lenvar)
	| Pointer(styp) ->
			generate_typesets classmap styp inpname
	| Unit -> raise (SkeletonGenerationException "Can't Unit typesets")
	| Fun(_, _) -> raise (SkeletonGenerationException "Cannot generate typesets from a fun")
	(* Everything else goes to itself.  *)
	| Bool -> SType(SBool(name_from_opt inpname))
	| Int16 -> SType(SInt(name_from_opt inpname))
	| Int32 -> SType(SInt(name_from_opt inpname))
	| Int64 -> SType(SInt(name_from_opt inpname))
	| UInt16 -> SType(SInt(name_from_opt inpname))
	| UInt32 -> SType(SInt(name_from_opt inpname))
	| UInt64 -> SType(SInt(name_from_opt inpname))
	| Float16 -> SType(SFloat(name_from_opt inpname))
	| Float32 -> SType(SFloat(name_from_opt inpname))
	| Float64 -> SType(SFloat(name_from_opt inpname))

let skeleton_type_lookup typemap names =
    List.map names (fun name -> generate_typesets typemap.classmap (Hashtbl.find_exn typemap.variable_map name) (Some(name)))

let rec repeat n item =
    match n with
    | 0 -> []
    | n -> item :: (repeat (n - 1) item)

(* Given a list of list of assignment sets, merge
them by the variables they assign to.  *)
let merge_skeleton_assigns (assigns: single_variable_binding_option_group list list list) =
	(* We do this by constructing a hash table, then putting it back into a list list. *)
	let joined = List.concat (List.concat assigns) in
	let lookuptbl = Hashtbl.create (module String) in
	(* List deconstruction.  *)
	let _ = List.map joined (fun assigns ->
		let name_entry = name_reference_list_to_string assigns.tovar_index_nesting in
		let existing_entries = Hashtbl.find lookuptbl name_entry in
		let new_entry = match existing_entries with
		| None -> assigns :: []
		| Some(x) -> assigns :: x
		in
		Hashtbl.set lookuptbl name_entry new_entry
	) in
	(* Now, create the list from the hashtbl.  *)
	Hashtbl.data lookuptbl

(* TODO --- We can reduce the number of bindings we need to check here. *)
let binding_check binding = true

(* Determines whether a type from_t is compatible with
   a type to_t --- that is, can we get the information
   to create a suitably-valued to_t with the information
			in a from_t.  *)
let rec compatible_types from_t to_t: bool =
	match from_t, to_t with
	(* TODO -- handle cross-conversion between bool and int? *)
	| SInt(nfrom), SInt(nto) -> true
	| SBool(nfrom), SBool(nto) -> true
	| SFloat(nfrom), SFloat(nto) -> true
	(* Sometimes, cross conversoin sbetween floats and its
	   are useful -- can we use heuristics to decide when this
	   is likely to be the case? *)
	| SFloat(nfrom), SInt(nto) -> true
	| SInt(nfrom), SFloat(sto) -> true
	(* There are all kinds of other conversions that could be matched,
	   just need to add them in here.  *)
	| _ -> false

(* Every output has to have either an input assigned to it,
   or no inputs assigned (in which case a constant can be
   used).  Generate all the mappings for one particular output. *)
(* Since this should be binding a single output variable, it
   does not produce a null name_binding, as the name of the output
   variable will be bound once the function ends.  Similarly
   for the skeleton_type_binding.  *)
(* This method is currently a bit of overkill for one output variable,
   since it was designed for many output variables.  However,
   it should eventually be modified to consider more than
   just binary conversions.  *)
and bindings_for (typesets_in: skeleton_type list) (output: skeleton_type): assignment_type list =
	(* Create all reasonable sets of bindings.  *)
	match typesets_in with
	| [] ->
			(* No matches possible from an enpty list of assigning
			   variables! *)
			[]
	| itypeset :: rtypesets_in ->
			(* If there is type overlap, then get that out. *)
            let types_compatible = compatible_types itypeset output in
			(* Try using this variable to assign to the output
			   variable and also try not using it.  *)
			(* With the assignments made here. *)
			let subtask_with_intersection: assignment_type list =
                (* The match wasn't complete, so skip *)
				if not types_compatible then
                    []
				else
                    (* The match was complete --- there is no point in recursing, this assignment
                       is good endouh. *)
                    [AssignVariable([name_refs_from_skeleton itypeset])]
			in
			(* Without the assignments made here. *)
			let subtask_without_intersection =
				bindings_for rtypesets_in output
			in
			subtask_with_intersection @ subtask_without_intersection

(* This is the difficult one.  It is hard to tell
   what the optimum set of bindings is, although
   it likely includes fairly low binding reuse.

   Need to get a set of assignments for each
   output. *)
and possible_bindings options direction constant_options_map (typesets_in: skeleton_dimension_group_type list) (types_out: skeleton_dimension_group_type list): single_variable_binding_option_group list list = 
    List.concat (List.map types_out (fun type_out ->
		(*  Make sure that all the possible assingments
		    have the same dimension.  *)
		match type_out with
		(* If this is a dimension type, we need to recurse
		   into the subtypes, which might also be dimension types.
		   (And then we need to find appropriate assignments for
		   those). *)
		| SArray(sarray_nam, array_subtyps, dim_options) ->
				let valid_dimensioned_typesets_in, dim_mappings  = List.unzip (List.filter_map typesets_in (fun intype ->
					match intype with
					| SArray(_, _, in_dim_options) ->
							(* Get the set of possible typevar
							   bindings that would make this mapping
							   possible.  *)
                            let dimoverlap = dimensions_overlap direction dim_options in_dim_options in
							Some(intype, dimoverlap)
					(* non-dimension types can't be included. *)
					| _ -> None
				)) in
				(* Get the STypes from these arrays, but keep the
				   dim_options.  *)
				let valid_undimensioned_typesets_in =
						List.map valid_dimensioned_typesets_in (fun intype ->
							match intype with
							| SArray(parent_array_name, artyp, dims) -> (parent_array_name, artyp)
							| _ -> raise (SkeletonGenerationException "")
						) in
				let () = if options.debug_generate_skeletons then
					Printf.printf "Recursing from ARRAY_ASSIGN using valid assignment vars %s\n"
						(skeleton_dimension_group_type_list_to_string (List.map valid_undimensioned_typesets_in (fun (a, b) -> b)))
				else () in
				(* Flatten the arstyles into a list.  *)
				let flattened_arr_stypes = flatten_stype_list array_subtyps in
				let flattened_undimensioned_typesets = List.map valid_undimensioned_typesets_in (fun (nam, typ) -> (nam, flatten_stype_list typ)) in
				let () = if options.debug_generate_skeletons then
					Printf.printf "Have %d assigns to generate.\n" (List.length flattened_undimensioned_typesets)
				else () in
				(* Recurse for the matches.  *)
				let recurse_assignments: single_variable_binding_option_group list list list =
					(* Put the undimensioned typelists back with their types.  *)
					List.map (List.zip_exn flattened_undimensioned_typesets dim_mappings) (fun ((arrnam, flattened_undimensioned_typeset), dim_mapping) ->
						let () = if options.debug_generate_skeletons then
							let () = Printf.printf "Looking at array with name %s\n" (name_reference_to_string arrnam) in
							let () = Printf.printf "Has flattened undimed typeset %s\n" (skeleton_dimension_group_type_list_to_string flattened_undimensioned_typeset) in
							()
						else () in
                        let () = assert ((List.length dim_mapping) > 0) in
						(* Get the possible bindings.  *)
						let poss_bindings: single_variable_binding_option_group list list =
							possible_bindings options direction constant_options_map flattened_undimensioned_typeset flattened_arr_stypes in
						let () = verify_single_binding_option_groups poss_bindings in
						(* Now add the required dimension mappings.  *)
						(* For each variable assignment *)
						let result = List.map poss_bindings (fun var_binds ->
							(* For each possible bind to that variable.  *)
							List.map var_binds (fun bind ->
								(* Add the array prefixes.  *)
								{
                                    fromvars_index_nesting = prepend_all arrnam bind.fromvars_index_nesting;
                                    tovar_index_nesting = sarray_nam :: bind.tovar_index_nesting;
                                    valid_dimensions_set = dim_mapping :: bind.valid_dimensions_set
                                }
							)
                        ) in
						let () = verify_single_binding_option_groups result in
						result
                    )
					in
				let concated_recurse_assignments = merge_skeleton_assigns recurse_assignments in
				let () = if options.debug_generate_skeletons then
					let () = Printf.printf "ARRAY_ASSIGN: For the flattened subtypes %s\n" (skeleton_dimension_group_type_list_to_string flattened_arr_stypes) in
					let () = Printf.printf "Found the following assignments %s\n" (double_binding_options_list_to_string concated_recurse_assignments) in
					let () = Printf.printf "Had the following types available to find these assignments %s\n" (skeleton_dimension_group_type_list_to_string typesets_in) in
					Printf.printf "Filtered these down to %s\n" (skeleton_dimension_group_type_list_to_string (List.map valid_undimensioned_typesets_in (fun (a, b) -> b)))
				else
					() in
				let () = verify_single_binding_option_groups concated_recurse_assignments in
				concated_recurse_assignments
		(* If this isn't an array, then allow any type
		   that isn't an array. *)
		| STypes(_) -> raise (SkeletonGenerationException "Need to flatten STypes out before getting mappings")
		| SType(stype_out) ->
				let zero_dimtypes = flatten_stypes typesets_in in
				let var_binds = bindings_for zero_dimtypes stype_out in
                let constbinds = 
                    (* Generate a list of suitable constant binds
                    for this varaible.  *)
                    get_plausible_constants_for constant_options_map stype_out in
                let all_binds = var_binds @ constbinds in
				let () = if (List.length all_binds) = 0 then
					let () = Printf.printf "Found no plausible binds for variable %s!\n%!" (name_reference_to_string (name_refs_from_skeleton stype_out)) in
					let () = Printf.printf "Hashtbl keys were %s\n" (String.concat (Hashtbl.keys constant_options_map)) in
					let () = Printf.printf "Number of constant entries was %d\n" (List.length (Hashtbl.find_exn constant_options_map (name_reference_to_string (name_refs_from_skeleton stype_out)))) in
					assert false
				else () in
				let () = if options.debug_generate_skeletons then
                    let () = Printf.printf "SINGLE_ASSIGN: Assiging to variable '%s' using bindings %s\n" (name_reference_to_string (name_refs_from_skeleton stype_out))
						((skeleton_dimension_group_type_list_to_string typesets_in )) in
                    let () = if (List.length all_binds) = 0 then
                        let () = Printf.printf "Found no possible bindings for variable %s%!\n" (name_reference_to_string (name_refs_from_skeleton stype_out)) in
                        ()
                    else () in
                    ()
				else () in
        (* The bindings_for doesn't create the whole skeleton_type_binding or
           name_binding types.  This needs to add the informtion about
           the out variable to each set to achieve that.  *)
			let results = [List.map all_binds (fun binding ->
			{
                fromvars_index_nesting = [binding];
                tovar_index_nesting = [name_refs_from_skeleton stype_out];
                valid_dimensions_set = [];
            })] in
			let () = verify_single_binding_option_groups results in
			results
		))

(* Given a list of variables, and an equally sized list of
   possible bindings, select one of the possible bindings
   for each variable.  *)
(* For now we try to create all possible bindings and filter them
   later.   May need a more efficient search strategy eventually.  *)
let rec find_possible_skeletons options (possible_bindings_for_var: single_variable_binding_option_group list list): skeleton_type_binding list =
    (* TODO --- Fix this function so it isn't empty *)
	let () = if options.debug_generate_skeletons then
		Printf.printf "Number of Varibales left to bind: %d\n" (List.length possible_bindings_for_var)
		else () in
	match (possible_bindings_for_var) with
	| [] -> [{bindings = []}] (* No bindings needed *)
	(* Create the basis for the binding sets we are gong to explore *)
    (* | binding_list :: [] -> List.map binding_list (fun b -> {
		bindings = [b]
	}) *)
	| binding :: bindings ->
			let () = if options.debug_generate_skeletons then
				let () = Printf.printf "Number of options for this variable is %d\n" (List.length binding) in
				let () = if List.length binding > 0 then
					Printf.printf "Working on binding from vars %s\n" (name_reference_list_to_string (List.hd_exn binding).tovar_index_nesting)
				else () in
				()
			else () in
			let subbindings = find_possible_skeletons options bindings in
			(* Include each possible set of bindings for this variable. *)
            (* Note that we can't just /not/ bind a variable, so we need to
               make sure that we don't skip a binding at any
               occasion.  *)
			List.concat (List.map binding (fun b -> List.map subbindings (fun sub -> { bindings = (b :: sub.bindings)})))

(* Given some variable that only has to be defined
and not assigned to, create a binding for it.  This
is more challenging that it seems at first maybe.
I'm not sure.  Doing it the simple way, may have
to flatten the define list and do it the more complicated
way (ie. work out which defs actually need to be defed
).  *)
let rec define_bindings_for direction valid_dimvars vs =
	List.map vs (fun v ->
		match v with
        | SArray(_, _, EmptyDimension) ->
                raise (SkeletonGenerationException "Can't have empty dim")
		| SArray(arnam, _, Dimension(dms)) ->
                let dimvar_bindings = dim_has_overlap direction [dms] valid_dimvars in
				[{
					(* May have to properly
					deal with array childer. *)
					tovar_index_nesting = [name_reference_base_name arnam; AnonymousName];
					fromvars_index_nesting = [];
                    valid_dimensions_set = [dimvar_bindings]
                }]
		| SType(SInt(n)) ->
				[{
					tovar_index_nesting = [name_reference_base_name n];
					fromvars_index_nesting = [AssignConstant(Int64V(0))];
                    valid_dimensions_set = []
                }]
		| SType(SBool(n)) ->
				[{
					tovar_index_nesting = [name_reference_base_name n];
					fromvars_index_nesting = [AssignConstant(BoolV(false))];
					valid_dimensions_set = []
				}]
		| SType(SFloat(n)) ->
				[{
					tovar_index_nesting = [name_reference_base_name n];
					fromvars_index_nesting = [AssignConstant(Float32V(0.0))];
                    valid_dimensions_set = []
                }]
        | STypes(ts) ->
                (* Think we can get away with nly defing
                the top level one.  Not 100% sure.  *)
                let sbase_names = (define_bindings_for direction valid_dimvars ts) in
                List.concat(
                    List.map sbase_names (remove_duplicates single_variable_binding_equal)
                )
    )

let possible_skeletons options possible_bindings_for_var =
	(* Get the skeletons for assigining to variables.  *)
	find_possible_skeletons options possible_bindings_for_var

let get_dimvars_used typeset =
    let with_dups =
        List.concat (
            List.map typeset (fun typ ->
                match typ with
                | SArray(_, _, EmptyDimension) -> []
                | SArray(_, _, Dimension(vs)) -> [vs]
                | _ -> []
            )
        )in
    remove_duplicates dimension_value_equal with_dups

let assign_and_define_bindings options direction constant_options_map typesets_in typesets_out typesets_define_only =
	(* Need to flatten the output typesets to make sure we only
	   look for one type at a type --- this takes the STypes([...])
	   and converts it to [...] *)
	let flattened_typesets_out = List.concat (List.map typesets_out flatten_stype_list) in
	(* Get a list of list of possible inputs for each
	   output.  This is type filtered, so the idea
	   is that it is sane.  *)
	let possible_bindings_list: single_variable_binding_option_group list list = possible_bindings options direction constant_options_map typesets_in flattened_typesets_out in
	let () = verify_single_binding_option_groups possible_bindings_list in
	(* Now, generate a set of empty assigns for the variables
	that don't have to have anything assigned to them.
	Perhaps we should use a type for this rather than just
	an empty list.  *)
    (* Toplevel dimvars.  *)
    let valid_dimvars = get_dimvars_used typesets_in in
	let define_bindings =
		define_bindings_for direction valid_dimvars typesets_define_only in
	List.concat [
			possible_bindings_list; define_bindings
		]

(* The algorithm should produce bindings from the inputs
   the outputs. *)
let binding_skeleton options direction typemap constant_options_map typesets_in typesets_out typesets_define =
	let possible_bindings_list = assign_and_define_bindings options direction constant_options_map typesets_in typesets_out typesets_define in
	(* Verify that there is exactly one variable per list, and that
	   there is not more than one list per variable.  *)
	(* Then, filter out various bindings that might not make any sense.  *)
	let sensible_bindings = List.filter possible_bindings_list binding_check in
	(* Finally, create some skeletons from those bindings.  *)
    (* Need to have one set of bindings for each output variable.  *)
	let possible_skeletons_list: skeleton_type_binding list = possible_skeletons options sensible_bindings in
	let sensible_skeletons = List.filter possible_skeletons_list skeleton_check in
	(* Debug info: *)
	if options.debug_generate_skeletons then
		(Printf.printf "Call to binding_skeleton\n";
		Printf.printf "Typesets in is %s\n" (skeleton_dimension_group_types_to_string typesets_in);
		Printf.printf "Typesets out is %s\n" (skeleton_dimension_group_types_to_string typesets_out);
		Printf.printf "Number of possible bindings is %d\n" (List.length possible_bindings_list);
		Printf.printf "Number of sensible bindings is %d\n" (List.length sensible_bindings);
		Printf.printf "Number of possible skeletons is %d\n" (List.length possible_skeletons_list);
		Printf.printf "Number of sensible skeletons is %d\n" (List.length sensible_skeletons);
		Printf.printf "Sensible skeletons are: %s\n" (skeleton_list_to_string sensible_skeletons);
		()
		
		)
	else
		();
	sensible_skeletons

(* Given the input classmap, IOSpec, and APISpec, generate
   the pre- and post-skeletons.  Pair them, and do some
   filtering to check for sanity.  *)
let generate_skeleton_pairs options typemap (iospec: iospec) (apispec: apispec) =
    (* Get the types of the varios input variables.  *)
    let livein_types = skeleton_type_lookup typemap iospec.livein in
    let livein_api_types = skeleton_type_lookup typemap apispec.livein in
    let liveout_api_types = skeleton_type_lookup typemap apispec.liveout in
    let liveout_types = skeleton_type_lookup typemap (iospec.liveout @ iospec.returnvar) in
    (* Get the types that are not livein, but are function args.  *)
    let define_only_api_types = skeleton_type_lookup typemap (set_difference Utils.string_equal apispec.funargs apispec.livein) in
	(* Also need to get any return types thare are not already
	defined.  *)
	let define_only_return_vars = set_difference Utils.string_equal iospec.returnvar iospec.funargs in
	let define_only_return_types = skeleton_type_lookup typemap (define_only_return_vars) in
    (* Get any constants that we should try for the pre-binds.  *)
    let constant_options_map = generate_plausible_constants_map options iospec.constmap apispec.validmap livein_types livein_api_types in
    (* Get constants that we should try for the post-binds
       Empty for now --- Can't really think a user function
       is likely to return a constant -- but maybe we should
       revisit, e.g. maybe there are things that return 0
       on success. *)
    let post_constant_options_map = Hashtbl.create (module String) in
    (* Now use these to create skeletons.  *)
	let pre_skeletons: skeleton_type_binding list = binding_skeleton options PreBinding typemap constant_options_map livein_types livein_api_types define_only_api_types in
    let post_skeletons = binding_skeleton options PostBinding typemap post_constant_options_map liveout_api_types liveout_types define_only_return_types in
	(* Flatten the skeletons that had multiple options for dimvars.  *)
	let flattened_pre_skeletons = flatten_skeleton options pre_skeletons in
	let flattened_post_skeletons = flatten_skeleton options post_skeletons in
	(* Do range-checks and input any value map functions.  *)
	let range_checked_pre_skeletons = rangecheck_skeletons options flattened_pre_skeletons iospec.rangemap apispec.validmap in
	let range_checked_post_skeletons = rangecheck_skeletons options flattened_post_skeletons apispec.validmap iospec.rangemap in
	(* Compute the range checks *)
	let range_progs = generate_range_checks_skeleton options typemap iospec apispec range_checked_pre_skeletons in
	(* Generate the range maps that can be used for input generation.  *)
	let input_maps = generate_input_ranges options iospec.rangemap apispec.validmap range_checked_pre_skeletons in
	let post_check_validmaps = generate_post_check_ranges options iospec.rangemap apispec.validmap range_checked_pre_skeletons in
    let maps = List.zip_exn input_maps post_check_validmaps in
    (* Do skeleton pairing *)
	let pre_skeletons_with_ranges = List.zip_exn range_progs range_checked_pre_skeletons in
	let pre_skeletons_with_ranges_and_inputs = List.zip_exn pre_skeletons_with_ranges maps in
    let all_skeleton_paris = List.cartesian_product pre_skeletons_with_ranges_and_inputs range_checked_post_skeletons in
    let skeleton_pair_objects = List.map all_skeleton_paris (fun (((rangecheck, pre), (inputmap, post_check_valid)), post) ->
        {
            pre = pre;
            post = post;
            rangecheck = rangecheck;
			inputmap = inputmap;
            post_check_validmap = post_check_valid;
			typemap = typemap;
        }
    ) in
    (* Do joint filtering *)
    let sensible_skeleton_pairs = List.filter skeleton_pair_objects (skeleton_pair_check options) in
	let () = if options.debug_generate_skeletons then
        (Printf.printf "Number of types (livein IO=%d, livein API=%d, liveout API=%d, liveout IO=%d)\n"
            (List.length livein_types) (List.length livein_api_types)
            (List.length liveout_api_types) (List.length liveout_types);
		Printf.printf "For this typemap:\n";
        Printf.printf "Number of pre-bindings generated is %d\n" (List.length range_checked_pre_skeletons);
        Printf.printf "Number of post-bindings generated is %d\n" (List.length range_checked_post_skeletons);
        Printf.printf "Number of skeletons generated is %d\n" (List.length skeleton_pair_objects);
        Printf.printf "Number of skeletons post-filtering is %d\n" (List.length sensible_skeleton_pairs);
		Printf.printf "Returnvars requiring a define are %s\n" (String.concat ~sep:", " define_only_return_vars);
        )
	else () in
	if options.debug_generate_skeletons then
        (
		Printf.printf "Liveout types are %s\n" (String.concat ~sep:", " (List.map liveout_types skeleton_dimension_group_type_to_string));
		Printf.printf "Livein types are %s\n" (String.concat ~sep:", " (List.map livein_types skeleton_dimension_group_type_to_string));
		Printf.printf "Liveout API types are %s\n" (String.concat ~sep:", " (List.map liveout_api_types skeleton_dimension_group_type_to_string));
		Printf.printf "Livein API types are %s\n" (String.concat ~sep:", " (List.map liveout_api_types skeleton_dimension_group_type_to_string));
        (* Currently no postbinding filtering visible in this function.
        Printf.printf "Number of pre-bindings post-filterings is %d" (List.length 
        Printf.printf "Number of post-bindings post-filterings is %d" (List.length 
        *)
        ())
    else
        ();
	if options.dump_skeletons then
		(Printf.printf "Pre bindings are: %s\n" (String.concat ~sep:"\nNEW PRE SKEL\n" (List.map range_checked_pre_skeletons (flat_skeleton_type_binding_to_string)));
		())
	else
		();
	(* Assert that all the pairs are valid structures, e.g. assigning once etc. *)
	let () = verify_skeleton_pairs options typemap iospec apispec sensible_skeleton_pairs in
    sensible_skeleton_pairs

let generate_all_skeleton_pairs opts typemaps iospec apispec =
	let result = List.concat (
		List.map typemaps (fun typemap ->
			generate_skeleton_pairs opts typemap iospec apispec
		)
	) in
	let () = if opts.debug_generate_skeletons then
		Printf.printf "Total number fo skeletons (across all typemaps) is %d\n" (List.length result)
	else ()
	in
	result
