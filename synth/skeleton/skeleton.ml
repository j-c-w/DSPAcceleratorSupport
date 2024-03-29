open Spec_definition;;
open Spec_utils;;
open Core;;
open Options;;
open Skeleton_flatten;;
open Skeleton_constants;;
open Skeleton_deduplicate;;
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

let binding_mode_to_string b =
    match b with
    | PostBinding -> "PostBinding"
    | PreBinding -> "PreBinding"

(* THis function turns a SType into a list of types
   by flattening them.  (i.e. SType(A,B,C) -> A,B,C*)
let rec flatten_stype_list stype =
	match stype with
	| SType(_) as t -> [t]
	| STypes(typs) -> List.concat (List.map typs ~f:flatten_stype_list)
	(* We don't touch the arrays, because those can't
	   come out of their array wrappers.  We do
	   need to make sure that the subtyps are flattened
	   but I've decided that that should happen right
	   before the call to inspect this type.  *)
	| SArray(name, subtyps, lenvar) as t -> [t]

let rec flatten_stype_probability_list stype =
    match stype with
    | Probability(SType(_), p) as t -> [t]
    | Probability(STypes(typs), p) -> List.concat (List.map typs ~f:(fun t -> flatten_stype_probability_list (Probability(t, p))))
    | Probability(SArray(name, subtyps, lenvar), p) as t -> [t]

let rec contains x ys =
	match ys with
	| [] -> false
	| y :: ys -> (x = y) || (contains x ys)

let get_plausible_constants_for options optsmap name =
	(* let () = Printf.printf "Looking for consts compatible with %s\n" (skeleton_type_to_string name) in
	let () = Printf.printf "table keys are %s\n " (String.concat ~sep:", " (Hashtbl.keys optsmap)) in *)
    match Hashtbl.find optsmap (name_reference_to_string (name_refs_from_skeleton name)) with
    | Some(opts) ->
			(* We should really have a better probabilistic likelyhood calculator -- as-is this calculation
			   might get pushed out by unlikely variables.  *)
            (* That said -- it's not super clear what the probability
            of this should be.  Perhaps it should be set by a flag
            that's rather than just set to the binding threshold
            as a proxy.  *)
            List.map opts ~f:(fun opt -> (options.binding_threshold, AssignConstant(opt)))
    | None -> []

let rec big_intersection lists =
	match lists with
	| [] -> []
	| [] :: ys -> []
	| (x :: xs) :: ys ->
			if (List.for_all ys ~f:(contains x)) then
				x :: (big_intersection (xs :: ys))
			else
				big_intersection (xs :: ys)

let variable_in_type var typ =
	match var, typ with
	| n1, SInt(n2) -> (name_reference_equal n1 n2)
	| n1, SBool(n2) -> (name_reference_equal n1 n2)
	| n1, SFloat(n2) -> (name_reference_equal n1 n2)
	| n1, SString(n2) -> (name_reference_equal n1 n2)

let rec flatten_stypes sty = 
	List.concat (List.filter_map sty
	~f:(fun ty -> match ty with
	| Probability(SType(x), prob) -> Some([(x, prob)])
	| Probability(STypes(x), prob) ->
            assert false (* Unimplemented? *)
			(* let sub_flattened = flatten_stypes (List.map x ~f:(fun l -> Probability(l, prob))) in
			Some(sub_flattened) *)
	| Probability(SArray(_, _, _), prob) -> None))

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
(* The returned list is a list of conditions on the binding.  *)
let dimvar_match x y =
	(* let () = Printf.printf "Matching %s and %s\n" (dimension_value_to_string x) (dimension_value_to_string y) in *)
	(* x is the accelerator var, y is the input var *)
    let result = match x, y with
	| DimVariable(vname1, DimEqualityRelation), DimVariable(vname2, DimEqualityRelation) ->
            Some(DimvarOneDimension(VarMatch(vname1, vname2, DimEqualityRelation)))
    | DimVariable(vname1, DimPo2Relation), DimVariable(vname2, DimPo2Relation) ->
            Some(DimvarOneDimension(VarMatch(vname1, vname2, DimEqualityRelation)))
    | DimVariable(vname1, DimEqualityRelation), DimVariable(vname2, DimPo2Relation) ->
            Some(DimvarOneDimension(VarMatch(vname1, vname2, DimPo2Relation)))
	| DimVariable(vname1, DimDivByRelation(x)), DimVariable(vname2, DimDivByRelation(y)) ->
			if x = y then
                Some(DimvarOneDimension(VarMatch(vname1, vname2, DimEqualityRelation)))
			else
				(* TODO --- infer another multiplication factor? *)
				None
	| DimVariable(vname1, DimEqualityRelation), DimVariable(vname2, DimDivByRelation(x)) ->
            Some(DimvarOneDimension(VarMatch(vname1, vname2, DimDivByRelation(x))))
            (* TODO -- do we need an inverse po2/mulby relation? *)
    | DimVariable(vname1, _), DimVariable(vname2, _) ->
            (* TODO --- does it do us any good to do a
            conversion here?  not sure.  *)
			None
	| DimConstant(c1), DimConstant(c2) ->
		if c1 = c2 then
			Some(DimvarOneDimension(ConstantMatch(c1)))
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
	let result = match y with
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
            (* Keep track of the original assignment in addition
             * to the generated constraints.  *)
			| Some(vs) -> (DimensionConstraints(vs, SingleDimension(x'))) :: (dimvar_contains direction x ys)
	in
	(* let () = Printf.printf "Result of dimvar contains is %s\n" (dimension_constraint_list_to_string result) in *)
	result

let rec dim_has_overlap direction x y =
	List.concat (List.map x ~f:(fun xel -> dimvar_contains direction xel y ))

(* /only/ match the inputs in the order they are given.
   This is used for the MultiDimension structs that can
   be matched to each other, but require that the elements
   are matched in order.

   Not clear what the best generic solution to this problem
   is, because the requirement of fixed orders is clearly
   too much, but also I'm not sure what the solution
   to the generic problem is.  *)
let rec dim_overlap_no_permute direction x y =
    match List.zip x y with
    | Ok(l) ->
            List.concat (
                List.map l ~f:(fun (x', y') ->
                    dimvar_contains direction x' [y']
                )
            )
    | Unequal_lengths -> []

let rec dimensions_overlap direction x y =
    (* let () =
        Printf.printf "Dimensions are given by %s and %s\n" (dimension_type_to_string x) (dimension_type_to_string y)
    in *)
    let result = match x, y with
	| EmptyDimension, _ -> []
	| _, EmptyDimension -> []
	(* No matches if the modse are different right? *)
	| SingleDimension(_), MultiDimension(_, _) -> []
	| MultiDimension(_, _), SingleDimension(_) -> []
	| SingleDimension(nrefs), SingleDimension(nrefs2) -> dim_has_overlap direction [nrefs] [nrefs2]
	(* TODO -- support other op styles.  *)
	| MultiDimension(nrefs1, DimMultiply), MultiDimension(nrefs2, DimMultiply) ->
			(
			match List.zip nrefs1 nrefs2 with
			(* Create an equality relation expectation for each variable --- TODO --- find a way to support
			the reflexivity of '*' *)
            | Ok(l) ->
                    (* Need to use the no permute option here, because
                    the permute option produces all pairwise elements,
                and we are alooking for a single complete mapping.  *)
                    (* (we could of course generate that form the pairwise mappings,
                    but not sure that's worth it ATM --- would introduce
                     a much higher cost for the longer length
                     expressions IMO.  *)
					let single_dims = dim_overlap_no_permute direction nrefs1 nrefs2 in
                    (* let () = Printf.printf "Input lists were: %s and %s\n" (dimension_value_list_to_string nrefs1) (dimension_value_list_to_string nrefs2) in *)
                    (* let () = Printf.printf "Number of single dims created is %d\n" (List.length single_dims) in *)
                    (* let () = Printf.printf "Single dims are %s\n" (dimension_constraint_list_to_string single_dims) in *)
					(* The single dims contain constraints
					a bunch of singledimensions --- need
					to recreate the structure of the multi-dimension.
					Note that this will need to change if
						there are more than DimMultiply modes.  *)
					let mappings, dim_types = List.fold ~init:([], []) ~f:(fun (matches, dtypes) -> fun dim_constraints ->
						(* let () = Printf.printf "Folding a dim constraints  that is %s\n" (dimension_constraint_to_string dim_constraints) in *)
						match dim_constraints with
						| DimensionConstraints(vmatch, dimtype) ->
                                let this_vmatch = match vmatch with
                                | DimvarOneDimension(d) -> [d]
                                (* Don't expect this case to come up. *)
                                | DimvarMultiDimension(ds) -> ds
                                in
                                let this_dim_value = match dimtype with
                                | EmptyDimension -> assert false (* Think this should be impossible. *)
                                | SingleDimension(d) -> [d]
                                (* Think this won't appear, if it does, need to think
                                about  how to handle the mode.  *)
                                | MultiDimension(d, mode) -> assert false
                                in
								this_vmatch @ matches, this_dim_value @ dtypes
					) single_dims in
					(* let () = Printf.printf "Produced mappings are %s and dim types %s\n" (one_dim_var_mapping_list_to_string mappings) (dimension_value_list_to_string dim_types) in *)
					(* Combine these into a single 
					   multi-dimension. *)
                    let gen_constraint = DimensionConstraints(DimvarMultiDimension(mappings), MultiDimension(dim_types, DimMultiply)) in
                    (* let () = Printf.printf "Generated teh following constraints: %s\n" (dimension_constraint_to_string gen_constraint) in *)
                    [gen_constraint]
			| Unequal_lengths -> []
			)
    in
    (* let () =
        Printf.printf "Computed overlap is %s\n"
            (dimvar_mapping_list_to_string result)
    in *)
    result

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
	| SType(SString(nr)) -> SType(SString(add_name_nr name nr))
    | STypes(subtyps) ->
            STypes(List.map subtyps ~f:(add_name name))
    | SArray(aname, subty, dimvar) ->
            (* I think? We don't have to do the dimvar here? *)
            SArray((add_name_nr name aname), subty, dimvar)

let name_from_opt opt =
    match opt with
    | None -> AnonymousName
    | Some(n) -> Name(n)

(* Generates the base typesets for a class. *)
let rec generate_typesets classmap inptype parentname inpname: skeleton_dimension_group_type =
    (* let () = Printf.printf "Looking at intput type %s, with inpname %s\n" (synth_type_to_string inptype) (match inpname with | None -> "None" | Some(inpname) ->  inpname) in *)
    let result = match inptype with
	| Struct(name) ->
			(
            let (members, structtypemap) = (match Hashtbl.find classmap name with
			| Some(ClassMetadata(mdata)) -> (mdata.members, mdata.typemap)
			| Some(StructMetadata(mdata)) -> (mdata.members, mdata.typemap)
            | None -> raise (SkeletonGenerationException ("Undefined type " ^ name)))
            in
            (* Use the struct typemap to find the types of the members. *)
            let subtypes = List.map members ~f:(fun mem -> (Hashtbl.find_exn structtypemap mem, mem)) in
            let input_variable_name = match inpname with
            | Some(vname) -> Name(vname)
            | None -> AnonymousName
            in
            let new_parentname = match parentname with
            | None -> Some(input_variable_name)
            | Some(supername) -> Some(name_reference_concat supername (input_variable_name))
            in
            (* But need to use the classmap to recurse. *)
			let types = List.map subtypes
				~f:(fun (memtyp, memname) -> (generate_typesets classmap memtyp new_parentname (Some(memname)))) in
			(* Prepend the class names that are for representing array indexes.   *)
			let class_ref_types =
				match inpname with
				| None -> types
				| Some(inpname) ->
					List.map types ~f:(fun ty -> add_name inpname ty)
			in
			STypes(class_ref_types))
	| Array(subtyp, lenvar) ->
			(* Do the subcall with no parentname --- that has to be split out into a dimvar.  *)
            let subtyps = generate_typesets classmap subtyp None None in
            (* So, when lenvars are assigned in the typemap, they are assigned
            relative to the base class.  This makes them 'contextless' in some sense,
            as it's not clear from the original assignment which instance they
            refer to.  To address that problem, we build up the len var here
            to refer to this particular instance.  *)
            let full_lenvar = match parentname with
            | None -> lenvar
            | Some(parent) ->
                    match lenvar with
                    | EmptyDimension -> assert false (* No idea WTF to do here.  *)
                    | SingleDimension(DimConstant(_)) -> lenvar (* Keep the constant -- that doesn't need the context prepending.  *)
                    | SingleDimension(DimVariable(nr, rel)) ->
                            (* Relation stays the same, but we prepend the context that this particular
                               instance exists within.  *)
                            SingleDimension(DimVariable(name_reference_concat parent nr, rel))
					| MultiDimension(vs, op) ->
							(* TODO --- extract the common code
							here and above. *)
							MultiDimension(List.map vs ~f:(fun v ->
								match v with
								| DimConstant(_) -> v
								| DimVariable(nr, rel) ->
										DimVariable(name_reference_concat parent nr, rel))
							, op)
            in
			(* This gives the array the name, so 'x' belongs to '[]'.  *)
			SArray(name_from_opt inpname, subtyps, full_lenvar)
	| Pointer(styp) ->
			generate_typesets classmap styp parentname inpname
	| Unit -> raise (SkeletonGenerationException "Can't Unit typesets")
	| Fun(_, _) -> raise (SkeletonGenerationException "Cannot generate typesets from a fun")
	(* Everything else goes to itself.  *)
	| Bool -> SType(SBool(name_from_opt inpname))
	| Int8 -> SType(SInt(name_from_opt inpname))
	| Int16 -> SType(SInt(name_from_opt inpname))
	| Int32 -> SType(SInt(name_from_opt inpname))
	| Int64 -> SType(SInt(name_from_opt inpname))
	| UInt8 -> SType(SInt(name_from_opt inpname))
	| UInt16 -> SType(SInt(name_from_opt inpname))
	| UInt32 -> SType(SInt(name_from_opt inpname))
	| UInt64 -> SType(SInt(name_from_opt inpname))
	| String -> SType(SString(name_from_opt inpname))
	| Float16 -> SType(SFloat(name_from_opt inpname))
	| Float32 -> SType(SFloat(name_from_opt inpname))
	| Float64 -> SType(SFloat(name_from_opt inpname))
    in
    (* let () = Printf.printf "Generated result types %s\n" (skeleton_dimension_group_type_to_string result) in *)
    result

let skeleton_type_lookup typemap names =
    List.map names ~f:(fun name -> generate_typesets typemap.classmap (Hashtbl.find_exn typemap.variable_map name) (None) (Some(name)))

let build_dimension_groups options inputs =
	(* So this function implements two key heuristics.  First,
	   it takes variables and groups them into 'likely' and 'unlikely'.
	   the current usecase for this is how long ago a variable
	   was defined (e.g. if it was livein, it is unlikely to be
	   an assignment for a liveout variable, but there is a small
	   chance it could be.)

	   Once these initial probabilites are calculated, the variable
	   map from the --binding-probabilities flag is passed in.
	   That modulates the likelyhood of the vairous variables.
	   *)

    match options.binding_specification with
    | Some(spec) ->
        List.concat (List.map inputs ~f:(fun v ->
			(* Flatten any STypes into SType --- not clear that
			this is really the best place to do this, but is
			required to enable interaction with the flat probability
			map.  I think the added complexity of a not-flat
			probability map will not make up for the bonus
			of doing the flattening elsewhere.  *)
			(* IIRC the real use for non-flat spaces is in
length inference, which has already happened.  *)
			let flat_types = flatten_stype_list v in
			List.map flat_types ~f:(fun v ->
				let tbl = match Hashtbl.find spec.probabilities (skeleton_dimension_group_type_to_id_string v) with
				| Some(sub_tbl) ->
						sub_tbl
				| None ->
						Hashtbl.create (module String)
				in
				let () = if options.debug_skeleton_probabilities then
					let () = Printf.printf "Looking up binding probability map for variable %s\n" ((skeleton_dimension_group_type_to_id_string v)) in
					let () = Printf.printf "It has sub-keys %s\n" (String.concat ~sep:", " (Hashtbl.keys tbl)) in
					()
					else () in
				Probability(v, tbl)
			)
        ))
    | None ->
            List.map inputs ~f:(fun v ->
                Probability(v, Hashtbl.create (module String))
            )

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
	let _ = List.map joined ~f:(fun assigns ->
		let name_entry = name_reference_list_to_string assigns.tovar_index_nesting in
		let existing_entries = Hashtbl.find lookuptbl name_entry in
		let new_entry = match existing_entries with
		| None -> assigns :: []
		| Some(x) -> assigns :: x
		in
		Hashtbl.set lookuptbl ~key:name_entry ~data:new_entry
	) in
	(* Now, create the list from the hashtbl.  *)
	Hashtbl.data lookuptbl

(* TODO --- We can reduce the number of bindings we need to check here. *)
let binding_check binding = true

(* Determines whether a type from_t is compatible with
   a type to_t --- that is, can we get the information
   to create a suitably-valued to_t with the information
			in a from_t.  *)
let rec compatible_types options from_t to_t: bool =
	match from_t, to_t with
	(* TODO -- handle cross-conversion between bool and int? *)
	| SInt(nfrom), SBool(nto) -> true
	| SBool(nfrom), SInt(nto) -> true
	| SInt(nfrom), SInt(nto) -> true
	| SBool(nfrom), SBool(nto) -> true
	| SFloat(nfrom), SFloat(nto) -> true
	| SString(nfrom), SString(nto) -> true
	(* Sometimes, cross conversoin sbetween floats and its
	   are useful -- can we integrate this into a sliding
	   scale (e.g. based on how many candidates have been
	   generated so far?) *)
	| SFloat(nfrom), SInt(nto) ->
			(
			match options.heuristics_mode with
			| GEMM -> false
			| FFT -> true
			)
	| SInt(nfrom), SFloat(sto) ->
			(
			match options.heuristics_mode with
			| GEMM -> false
			| FFT -> true
			)
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
and bindings_for options (typesets_in: (skeleton_type * ((string, float) Hashtbl.t)) list) (output: skeleton_type): (float * assignment_type) list =
	(* Create all reasonable sets of bindings.  *)
	match typesets_in with
	| [] ->
			(* No matches possible from an enpty list of assigning
			   variables! *)
			[]
	| (itypeset, pmap) :: rtypesets_in ->
			(* If there is type overlap, then get that out. *)
            let types_compatible = compatible_types options itypeset output in
            let iprobability = match Hashtbl.find pmap (skeleton_type_to_id_string output) with
            | None -> options.binding_threshold (* Default to binding_threshold so that you don't have to specify everything *)
            | Some(p) -> p
            in
			let () = if options.debug_skeleton_probabilities then
				Printf.printf "Within variable map for %s, looking up variable %s --- found probability %f\n" (skeleton_type_to_id_string itypeset) (skeleton_type_to_id_string output) iprobability else () in
			(* Try using this variable to assign to the output
			   variable and also try not using it.  *)
			(* With the assignments made here. *)
			let subtask_with_intersection: (float * assignment_type) list =
                (* The match wasn't complete, so skip *)
				if not types_compatible then
                    []
				else
                    (* The match was complete --- there is no point in recursing, this assignment
                       is good endouh. *)
                    [(iprobability, AssignVariable([name_refs_from_skeleton itypeset]))]
			in
			(* Without the assignments made here. *)
			let subtask_without_intersection =
				bindings_for options rtypesets_in output
			in
			subtask_with_intersection @ subtask_without_intersection

(* This is the difficult one.  It is hard to tell
   what the optimum set of bindings is, although
   it likely includes fairly low binding reuse.

   Need to get a set of assignments for each
   output. *)
and possible_bindings options direction constant_options_map (typesets_in: skeleton_dimension_probabilistic_group_type list) (types_out: skeleton_dimension_group_type list): single_variable_binding_option_group list list = 
	let result = (List.map types_out ~f:(fun type_out ->
		(*  Make sure that all the possible assingments
		    have the same dimension.  *)
		match type_out with
		(* If this is a dimension type, we need to recurse
		   into the subtypes, which might also be dimension types.
		   (And then we need to find appropriate assignments for
		   those). *)
		| SArray(sarray_nam, array_subtyps, dim_options) ->
				let valid_dimensioned_typesets_in, dim_mappings  = List.unzip (List.filter_map typesets_in ~f:(fun intype ->
                    (* let () = Printf.printf "Variable name is %s\n" (skeleton_dimension_probabilistic_group_type_to_string intype) in *)
					match intype with
					| Probability(SArray(_, _, in_dim_options), p) ->
							(* Get the set of possible typevar
							   bindings that would make this mapping
							   possible.  *)
							(* let () = Printf.printf "In Dim Options are %s\n" (dimension_type_to_string in_dim_options) in *)
                            let dimoverlap = dimensions_overlap direction dim_options in_dim_options in
                            (* let () = Printf.printf "Dim overlap is %s\n" (dimension_constraint_list_to_string dimoverlap) in *)
							(
							match dimoverlap with
							| [] -> None
							| _ ->
								Some(intype, dimoverlap)
							)
                    | Probability(STypes(_), _) -> raise (SkeletonGenerationException "Unexepcted STypes --- need to be flattened")
					(* non-dimension types can't be included. *)
					| _ -> None
				)) in
				(* Get the STypes from these arrays, but keep the
				   dim_options.  *)
				let valid_undimensioned_typesets_in =
						List.map valid_dimensioned_typesets_in ~f:(fun intype ->
							match intype with
							| Probability(SArray(parent_array_name, artyp, dims), probability) -> (parent_array_name, artyp, probability)
							| _ -> raise (SkeletonGenerationException "")
						) in
				let () = if options.debug_generate_skeletons then
					Printf.printf "Recursing from ARRAY_ASSIGN (%s) using valid assignment vars %s\n"
                        (name_reference_to_string sarray_nam)
						(skeleton_dimension_group_type_list_to_string (List.map valid_undimensioned_typesets_in ~f:(fun (a, b, p) -> b)))
				else () in
				(* Flatten the arstyles into a list.  *)
				let flattened_arr_stypes = flatten_stype_list array_subtyps in
				let flattened_undimensioned_typesets = List.map valid_undimensioned_typesets_in ~f:(fun (nam, typ, prob) -> (nam, flatten_stype_list typ, prob)) in
				let () = if options.debug_generate_skeletons then
                    let () = Printf.printf "unflattened is %s\n" (skeleton_dimension_group_type_to_string array_subtyps) in
                    let () = Printf.printf "flattened is %s\n" (skeleton_dimension_group_type_list_to_string flattened_arr_stypes) in
					Printf.printf "Have %d assigns to generate.\n" (List.length flattened_undimensioned_typesets)
				else () in
				(* Recurse for the matches.  *)
				let recurse_assignments: single_variable_binding_option_group list list list =
					(* Put the undimensioned typelists back with their types.  *)
					List.map (List.zip_exn flattened_undimensioned_typesets dim_mappings) ~f:(fun ((arrnam, flattened_undimensioned_typeset, prob_map), dim_mapping) ->
						let () = if options.debug_generate_skeletons then
							let () = Printf.printf "Looking at array with name %s\n" (name_reference_to_string arrnam) in
							let () = Printf.printf "Has flattened undimed typeset %s%!\n" (skeleton_dimension_group_type_list_to_string flattened_undimensioned_typeset) in
							()
						else () in
                        let () = assert ((List.length dim_mapping) > 0) in
						let flattened_undimensioned_probability_typeset =
							List.map flattened_undimensioned_typeset ~f:(fun l -> Probability(l, prob_map)) in
						let () = if options.debug_generate_skeletons then
							Printf.printf "Generated %d probability_types %!\n" (List.length flattened_undimensioned_probability_typeset) else () in
						(* Get the possible bindings.  *)
						let poss_bindings: single_variable_binding_option_group list list =
							possible_bindings options direction constant_options_map flattened_undimensioned_probability_typeset flattened_arr_stypes in
						let () = if options.debug_generate_skeletons then
                            let () = Printf.printf "Generated %d possible bindings %!\n" (List.length poss_bindings) in
                            let () = Printf.printf "Possible binding options are %s\n" (single_variable_binding_list_list_to_string poss_bindings) in
						() else () in
						let () = verify_single_binding_option_groups poss_bindings in
						(* Now add the required dimension mappings.  *)
						(* For each variable assignment *)
						let result = List.map poss_bindings ~f:(fun var_binds ->
							(* For each possible bind to that variable.  *)
							List.map var_binds ~f:(fun bind ->
								let fromvars = prepend_all arrnam bind.fromvars_index_nesting in
								let tovars = sarray_nam :: bind.tovar_index_nesting in
                                (* Note this recomputes the probability over all the fvars *)
								let probs = List.map fromvars ~f:(fun fvar ->
                                    (* TODO --- Why does this assignment come from tovars?  It should come from fvar.  Not
                                     Clear where the probability map is getting flipped.*)
									let hashmapname = assignment_type_to_id_string (AssignVariable(tovars)) in
                                    let () = if options.debug_skeleton_probabilities then
										let () = Printf.printf "Within variable map for %s, looking up variable '%s'\n" (name_reference_list_to_string tovars) (hashmapname) in
									let () = Printf.printf "Prob map keps are %s\n" (String.concat ~sep:"," (Hashtbl.keys prob_map)) in
									()
									else () in
									let prob = match Hashtbl.find prob_map hashmapname with
									| Some(p) -> p
									| None ->
											let () = if options.debug_skeleton_probabilities then
												Printf.printf "Variable binding not found, falling back to binding_threshold\n"
											else ()
											in
											options.binding_threshold
									in
									prob
								) in
                                let overall_probability = List.fold ~init:1.0 ~f:(fun v1 -> (fun v2 -> v1 *. v2)) probs in
                                let () = if options.debug_skeleton_probabilities then
									let () = Printf.printf "Binding %s to %s has computed probability %f\n" (assignment_type_list_to_string fromvars) (name_reference_list_to_string tovars) (overall_probability) in
								let () = Printf.printf "Have dim mappings %s\n" (dimension_constraint_list_list_to_string (dim_mapping :: bind.dimensions_set)) in
								() else ()
                                in
								(* Add the array prefixes.  *)
								{
                                    fromvars_index_nesting = fromvars;
                                    tovar_index_nesting = tovars;
                                    dimensions_set = dim_mapping :: bind.dimensions_set;
									probability = overall_probability
                                }
							)
                        ) in
						let () = verify_single_binding_option_groups result in
                        (* let () = Printf.printf "Result with added dim mappings is %s\n" (single_variable_binding_list_list_to_string result) in *)
						result
                    )
					in
				let concated_recurse_assignments = merge_skeleton_assigns recurse_assignments in
				let () = if options.debug_generate_skeletons then
						let () = Printf.printf "Merged assimengs are %s\n" (single_variable_binding_list_list_to_string concated_recurse_assignments) in
					let () = Printf.printf "ARRAY_ASSIGN: For the flattened subtypes %s\n" (skeleton_dimension_group_type_list_to_string flattened_arr_stypes) in
                    let () = Printf.printf "Found %d options\n" (List.length flattened_arr_stypes) in
					let () = Printf.printf "Found the following assignments %s\n" (double_binding_options_list_to_string concated_recurse_assignments) in
					let () = Printf.printf "Had the following types available to find these assignments %s\n" (skeleton_dimension_probabilistic_group_type_list_to_string typesets_in) in
					Printf.printf "Filtered these down to %s\n%!" (skeleton_dimension_group_type_list_to_string (List.map valid_undimensioned_typesets_in ~f:(fun (a, b, p) -> b)))
				else
					() in
				let () = verify_single_binding_option_groups concated_recurse_assignments in
				if ((List.length concated_recurse_assignments) = 0) then
					[[]]
				else
					concated_recurse_assignments
		(* If this isn't an array, then allow any type
		   that isn't an array. *)
		| STypes(_) -> raise (SkeletonGenerationException "Need to flatten STypes out before getting mappings")
		| SType(stype_out) ->
				let zero_dimtypes = flatten_stypes typesets_in in
				let var_binds = bindings_for options zero_dimtypes stype_out in
                (* let () = Printf.printf "Var binds are %s\n" (assignment_type_list_to_string (List.map var_binds ~f:(fun (_, b) -> b))) in *)
                let constbinds = 
                    (* Generate a list of suitable constant binds
                    for this varaible.  *)
                    get_plausible_constants_for options constant_options_map stype_out in
                let all_binds = var_binds @ constbinds in
				let () = if (List.length all_binds) = 0 then
					let () = Printf.printf "Found no plausible binds for variable %s!\n%!" (name_reference_to_string (name_refs_from_skeleton stype_out)) in
					let () = Printf.printf "Hashtbl keys were %s\n" (String.concat (Hashtbl.keys constant_options_map)) in
					let () = Printf.printf "Number of constant entries was %d\n" (List.length (Hashtbl.find_exn constant_options_map (name_reference_to_string (name_refs_from_skeleton stype_out)))) in
					assert false
				else () in
				let () = if options.debug_generate_skeletons then
                    let () = Printf.printf "SINGLE_ASSIGN: Assiging to variable '%s' using bindings %s\n" (name_reference_to_string (name_refs_from_skeleton stype_out))
						((skeleton_dimension_probabilistic_group_type_list_to_string typesets_in )) in
                    let () = if (List.length all_binds) = 0 then
                        let () = Printf.printf "Found no possible bindings for variable %s%!\n" (name_reference_to_string (name_refs_from_skeleton stype_out)) in
                        ()
                    else () in
                    ()
				else () in
        (* The bindings_for doesn't create the whole skeleton_type_binding or
           name_binding types.  This needs to add the informtion about
           the out variable to each set to achieve that.  *)
			let results = [List.map all_binds ~f:(fun (prob, binding) ->
			{
                fromvars_index_nesting = [binding];
                tovar_index_nesting = [name_refs_from_skeleton stype_out];
                dimensions_set = [];
				probability = prob;
            })] in
			let () = verify_single_binding_option_groups results in
            (* let () = Printf.printf "final bindigns results are %s\n" (single_variable_binding_list_list_to_string results) in *)
			results
		))
	in
	(* let () = Printf.printf "Result size is %d, catted is %d\n" (List.length result) (List.length (List.concat result)) in *)
	(* let () = Printf.printf "Have the following number of options for each leelent %s\n" (String.concat ~sep:"," (List.map result (fun r -> string_of_int (List.length r)))) in *)
	List.concat (result)

let in_binding (binding: single_variable_binding_option_group) sub =
	List.exists sub.bindings ~f:(fun b ->
		(* Check each fromvar --- we only check generate
		 singleton fromvar lists right now, but this
		 should perhaps be a for_all if we generate
		 more than one fromvar. *)
		List.exists b.fromvars_index_nesting ~f:(fun assignment ->
			let this_assignment = binding.fromvars_index_nesting in

			(* Likewise with this list.exists. *)
			List.exists this_assignment ~f:(fun other_assignment ->
				(* We do this check because assignment_type_equal
				will return true for the same two constants.
				In this case, it's probably OK to have the same
				two constants --- we only check for variable
				equalities. *)
				match assignment with
				| AssignConstant(_) -> false
				| _ -> assignment_type_equal assignment other_assignment
			)
		)
	)

(* Given a list of variables, and an equally sized list of
   possible bindings, select one of the possible bindings
   for each variable.  *)
(* For now we try to create all possible bindings and filter them
   later.   May need a more efficient search strategy eventually.  *)
let rec find_possible_skeletons options (possible_bindings_for_var: single_variable_binding_option_group list list): skeleton_type_binding list =
    (* TODO --- Fix this function so it isn't empty *)
	let () = if options.debug_generate_skeletons then
		Printf.printf "Number of Varibales left to bind: %d\n%!" (List.length possible_bindings_for_var)
		else () in
	match (possible_bindings_for_var) with
	| [] -> [{bindings = []}] (* No bindings needed *)
	(* Create the basis for the binding sets we are gong to explore *)
    (* | binding_list :: [] -> List.map binding_list ~f:(fun b -> {
		bindings = [b]
	}) *)
	| binding :: bindings ->
			let () = if options.debug_generate_skeletons then
				let () = Printf.printf "Number of options for this variable is %d\n" (List.length binding) in
				let () = if List.length binding > 0 then
					Printf.printf "Working on binding from vars %s\n%!" (name_reference_list_to_string (List.hd_exn binding).tovar_index_nesting)
				else () in
				()
			else () in
			let subbindings = find_possible_skeletons options bindings in
			(* Include each possible set of bindings for this variable. *)
            (* Note that we can't just /not/ bind a variable, so we need to
               make sure that we don't skip a binding at any
               occasion.  *)
			List.concat (List.map binding ~f:(fun b ->
				List.filter_map subbindings ~f:(fun sub ->
					if (in_binding b sub) then
					(* Only produce the new binding
					if the variable that is assigned from
					hasn't been used already.  This is
					a crappy place to have this heuristic,
					because it doesn't fit with the generate/filter
					pattern used elsewhere.  However, without
					it here, these lists can get waay too long
					to be scalable.  *)
						None
					else
						Some({
                            bindings = (b :: sub.bindings)
						})
				)
				)
			)

(* Given some variable that only has to be defined
and not assigned to, create a binding for it.  This
is more challenging that it seems at first maybe.
I'm not sure.  Doing it the simple way, may have
to flatten the define list and do it the more complicated
way (ie. work out which defs actually need to be defed
).  *)
let rec define_bindings_for options direction valid_dimvars vs =
	List.map vs ~f:(fun v ->
		match v with
		| SArray(_, _, _) ->
				(* Do a sub-match on the particular type of
				the diemsion, which determiens which bindings
				we actually use for length variables.  *)
				let arnam, overlap = match v with
				| SArray(_, _, EmptyDimension) ->
						raise (SkeletonGenerationException "Can't have empty dim")
				| SArray(arnam, _, SingleDimension(dms)) ->
						let dimvar_bindings = dim_has_overlap direction [dms] valid_dimvars in
						arnam, dimvar_bindings
				| SArray(arnam, _, MultiDimension(dms, op)) ->
						let dimvar_bindings = dim_has_overlap direction dms valid_dimvars in
						arnam, dimvar_bindings
				| _ -> assert false (* Not possible --- must be sarray.  *)
				in
					let () = if options.debug_generate_skeletons then
						let () = Printf.printf "Got potential dimvars %s\n" (dimension_value_list_to_string valid_dimvars) in
					() else () in
					let () =
						if (List.length overlap) = 0 then
							let () = Printf.printf "Define binding has no valid dim vars.\n" in
							let () = Printf.printf "Direction is %s\n" (binding_mode_to_string direction) in
							(* TODO -- re-insert that debug statement. *)
							(* let () = Printf.printf "array dms is %s\n" (dimension_type_to_string (Dimension(dms))) in *)
							let () = Printf.printf "Valid dimvars is %s\n" (dimension_value_list_to_string valid_dimvars) in
							assert false
							else ()
							in
						let () = (assert (List.length overlap > 0)) in
						[{
							(* May have to properly
							deal with array childer. *)
							tovar_index_nesting = [name_reference_base_name arnam; AnonymousName];
							fromvars_index_nesting = [];
							dimensions_set = [overlap];
							probability = 1.0 (* note that we usually do 0 as default probabilities, but these
							are probabilities of defines, which should be 1.0 --- they're usually set because
							no alternative is needed.  *)
								}]
		| SType(SInt(n)) ->
				[{
					tovar_index_nesting = [name_reference_base_name n];
					fromvars_index_nesting = [AssignConstant(Int64V(0))];
					dimensions_set = [];
					probability = 1.0
                }]
		| SType(SBool(n)) ->
				[{
					tovar_index_nesting = [name_reference_base_name n];
					fromvars_index_nesting = [AssignConstant(BoolV(false))];
					dimensions_set = [];
					probability = 1.0
				}]
		| SType(SFloat(n)) ->
				[{
					tovar_index_nesting = [name_reference_base_name n];
					fromvars_index_nesting = [AssignConstant(Float32V(0.0))];
					dimensions_set = [];
					probability = 1.0
                }]
		| SType(SString(n)) ->
				(* So this may be backend-dependent.  Since C is currently the
				   supported backend it is true.

				   This is a consequence of the decision to make strings length-independent
				   while in C they really aren't.

				   The problem is that a typical C string is going
				   to be null-termianted, so a string copy should
				   look different from an array copy (as it may
				   not come with an assiciated length parameter).

				   Anyway, hopefully this no-defining of SString types
				   isn't too much of a limitation.  *)
				raise (SkeletonGenerationException "Can't generate define for string type")
				(* TODO -- may this should just return []? and not crash? *)
        | STypes(ts) ->
                (* Think we can get away with nly defing
                the top level one.  Not 100% sure.  *)
                let sbase_names = (define_bindings_for options direction valid_dimvars ts) in
                List.concat(
                    List.map sbase_names ~f:(remove_duplicates single_variable_binding_equal)
                )
    )

let possible_skeletons options possible_bindings_for_var =
	(* Get the skeletons for assigining to variables.  *)
	let res = find_possible_skeletons options possible_bindings_for_var in
	(* let () = Printf.printf "Finsihed execuging find possible skeletons\n%!" in *)
	res

let rec get_dimvars_used typeset =
    (* let () = Printf.printf "input typeset is %s\n" (skeleton_dimension_probabilistic_group_type_list_to_string typeset) in *)
    let with_dups =
        List.concat (
            List.map typeset ~f:(fun typ ->
                let dimvars = match typ with
                | Probability(SArray(_, _, EmptyDimension), _) -> []
                | Probability(SArray(_, _, SingleDimension(vs)), _) -> [vs]
				| Probability(SArray(_, _, MultiDimension(vs, op)), _) -> vs
                | Probability(STypes(subtype), p) ->
                        get_dimvars_used (List.map subtype ~f:(fun s -> Probability(s, p)))
                | _ -> [] in
                dimvars
            )
        )in
    let result = remove_duplicates dimension_value_equal with_dups in
    (* let () = Printf.printf "Length variables from typeset are %s\n" (dimension_value_list_to_string result) in *)
    result

(* So this is the function that tries to hit a tradeoff between compile
   time and odds of success.  *)
(* The aim is to try more things if thre are lots of likely things,
   and if there are no likely things, then to pick some unlikely ones.  *)
let cut_unlikely_bindings options binds =
	(* TODO --- come up with a better way of tracking this.  *)
	let _ =
		if options.debug_generate_skeletons then
			Printf.printf "cutting unlikely bindings: input bindings are %s\n"
				(single_variable_binding_list_to_string binds)
		else ()
	in
	let max_threshold_difference = options.binding_threshold in
	let compare = (fun (a: single_variable_binding_option_group) -> fun (b: single_variable_binding_option_group) ->
		Float.compare a.probability b.probability
	) in
    let sorted = List.rev (List.sort binds ~compare:compare) in
    (* Cut off when the threshold difference is too big from the first
    element.  *)
    let result = match sorted with
       | [] -> []
       | head :: rest ->
               let head_prob = head.probability in
			   let () = if options.debug_skeleton_probabilities then
				   Printf.printf "Most likely binding is %f, meaning threshold is %f\n" (head_prob) (head_prob -. max_threshold_difference)
			   else () in
               let value_check = (fun r -> ((Float.compare (head_prob -. max_threshold_difference) r.probability) = -1)) in
               (* Want to take all the elements that have prob greater than the prob diff.  *)
               head :: (List.take_while rest ~f:value_check)
               in
	let _ =
		if options.debug_generate_skeletons then
			let () = Printf.printf "Post cutting binding are %s\n"
				(single_variable_binding_list_to_string result) in
			Printf.printf "Number of bindings cut are %d\n" ((List.length sorted) - (List.length result))
		else ()
	in
	result

let assign_and_define_bindings options direction constant_options_map typesets_in typesets_out typesets_define_only =
	(* Need to flatten the output typesets to make sure we only
	   look for one type at a type --- this takes the STypes([...])
	   and converts it to [...] *)
	let flattened_typesets_out = List.concat (List.map typesets_out ~f:flatten_stype_list) in
    let () =
        if options.debug_skeleton_multiple_lengths_filter then
			Printf.printf "After flattenening, the output types are %s\n" (skeleton_dimension_group_types_to_string flattened_typesets_out)
        else ()
    in
    let flattened_typesets_in = List.concat (List.map typesets_in ~f:flatten_stype_probability_list) in
	(* Get a list of list of possible inputs for each
	   output.  This is type filtered, so the idea
	   is that it is sane.  *)
	let possible_bindings_list = possible_bindings options direction constant_options_map flattened_typesets_in flattened_typesets_out in
	let () = assert ((List.length flattened_typesets_out) = (List.length possible_bindings_list)) in
    let () =
        if options.debug_skeleton_multiple_lengths_filter then
            let () = Printf.printf "generated possible bindings\n%!" in
			let () = Printf.printf "Possible bindings list is %d long\n" (List.length possible_bindings_list) in
            let () = Printf.printf "After Generation, Possible binding options are %s\n" (single_variable_binding_list_list_to_string possible_bindings_list) in
        () else ()
    in
	let likely_bindings: single_variable_binding_option_group list list =
		(* Cut bindings per element *)
		List.map possible_bindings_list ~f:(cut_unlikely_bindings options)
	in
	let () = if options.debug_skeleton_multiple_lengths_filter then
		let () = Printf.printf "Likely bindings are %s\n" (single_variable_binding_list_list_to_string possible_bindings_list) in
		() else () in
	let () = verify_single_binding_option_groups possible_bindings_list in
	(* Now, generate a set of empty assigns for the variables
	that don't have to have anything assigned to them.
	Perhaps we should use a type for this rather than just
	an empty list.  *)
    (* Toplevel dimvars.  *)
    let valid_dimvars = get_dimvars_used typesets_in in
	let define_bindings =
		define_bindings_for options direction valid_dimvars typesets_define_only in
    let result = List.concat [
			likely_bindings; define_bindings
		]
    in

	let () =
		if options.debug_skeleton_multiple_lengths_filter then
			let () = Printf.printf "Found possible bindings for %s\n"  (skeleton_dimension_group_types_to_string flattened_typesets_out) in
			let () = Printf.printf "Have the following variables %d\n" (List.length likely_bindings) in
			let () = Printf.printf "Have the following number of options for each variable %s\n" (String.concat ~sep:"," (List.map likely_bindings ~f:(fun r -> string_of_int (List.length r)))) in
			() else ()
	in
    let _ =
        List.map result ~f:(fun b ->
            List.map b ~f:(fun c ->
                match c.dimensions_set with
                | [] -> ()
                | x :: xs -> (assert (List.length x > 0))
            )
        )
    in
    result

(* The algorithm should produce bindings from the inputs
   the outputs. *)
let binding_skeleton options direction typemap constant_options_map typesets_in typesets_out typesets_define =
	let possible_bindings_list = assign_and_define_bindings options direction constant_options_map typesets_in typesets_out typesets_define in
	let () = if options.debug_generate_skeletons then
        let () = Printf.printf "Done generating possible bindings (%d possible)\n%!" (List.length possible_bindings_list) in
        let () = Printf.printf "In BindingSkeleton: Possible Bindings list is %s\n" (single_variable_binding_list_list_to_string possible_bindings_list) in
        () else ()
	in
	(* Verify that there is exactly one variable per list, and that
	   there is not more than one list per variable.  *)
	(* Then, filter out various bindings that might not make any sense.  *)
	let sensible_bindings = List.filter possible_bindings_list ~f:binding_check in
	let () = if options.debug_generate_skeletons then
		Printf.printf "Done filtering possible bindings (%d sensible)\n%!" (List.length sensible_bindings)
	else ()
	in
	(* Finally, create some skeletons from those bindings.  *)
    (* Need to have one set of bindings for each output variable.  *)
	let possible_skeletons_list: skeleton_type_binding list = possible_skeletons options sensible_bindings in
	let () = if List.length possible_skeletons_list > 4_000_000 then
		raise (SkeletonGenerationException "Error: Generated more than 1 million skeletons --- this will take too long to process, please use a probabilities.json file (--probabilities) to reduce the search space")
	else () in
	let () = if options.debug_generate_skeletons then
		Printf.printf "Done generating possible skeletons (%d of them)\n%!" (List.length possible_skeletons_list)
		(* let () = Printf.printf "Intermediate skeletons list is %s\n" (skeleton_type_binding_list_to_string possible_skeletons_list) in *)
	else ()
	in
	let sensible_skeletons = List.filter possible_skeletons_list ~f:(skeleton_check options) in
	(* Debug info: *)
	if options.debug_generate_skeletons then
		(Printf.printf "Call to binding_skeleton\n";
		Printf.printf "Typesets in is %s\n" (skeleton_dimension_probabilistic_group_type_list_to_string typesets_in);
		Printf.printf "Typesets out is %s\n" (skeleton_dimension_group_types_to_string typesets_out);
		Printf.printf "Number of possible bindings is %d\n" (List.length possible_bindings_list);
		Printf.printf "Number of sensible bindings is %d\n" (List.length sensible_bindings);
		Printf.printf "Number of possible skeletons is %d\n" (List.length possible_skeletons_list);
		Printf.printf "Number of sensible skeletons is %d\n" (List.length sensible_skeletons);
		Printf.printf "Possible bindings are %s\n" (single_variable_binding_list_list_to_string possible_bindings_list);
		Printf.printf "Sensible bindings are %s\n" (skeleton_type_binding_list_to_string possible_skeletons_list);
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
    let liveout_types = skeleton_type_lookup typemap (Utils.remove_duplicates Utils.string_equal (iospec.returnvar @ iospec.liveout)) in
	let () = if options.debug_generate_skeletons then
		let () = Printf.printf "Liveout types are %s\n" (skeleton_dimension_group_type_list_to_string liveout_types) in
		let () = Printf.printf "Livein types are %s\n" (skeleton_dimension_group_type_list_to_string livein_types) in
		let () = Printf.printf "Livein types (API) are %s\n" (skeleton_dimension_group_type_list_to_string livein_api_types) in
		let () = Printf.printf "Liveout types (API) are %s\n" (skeleton_dimension_group_type_list_to_string liveout_api_types) in
		() else () in
    (* Get the types that are not livein, but are function args.  *)
    let define_only_api_types = skeleton_type_lookup typemap (set_difference Utils.string_equal apispec.funargs apispec.livein) in
    (* Get any constants that we should try for the binds.  *)
    let constant_options_map = generate_plausible_constants_map options iospec.constmap apispec.validmap apispec.defaultmap livein_types (livein_api_types @ liveout_types) in
    (* Now use these to create skeletons.  *)
	(* Prebinds *)
	let prebind_inputs = build_dimension_groups options livein_types  in
	(* let () = Printf.printf "Prebind inputs are %s\n" (skeleton_dimension_probabilistic_group_type_list_to_string prebind_inputs) in *)
	let pre_skeletons: skeleton_type_binding list = binding_skeleton options PreBinding typemap constant_options_map prebind_inputs livein_api_types define_only_api_types in
	(* let () = Printf.printf "Unflattened Pre skeletons are %s\n" (skeleton_type_binding_list_to_string pre_skeletons) in *)
	(* Postbinds *)
	(* Aim is to get the types bound using the liveout api types, but with the original
	livein types being used if required.  *)
	let postbind_inputs = build_dimension_groups options (liveout_api_types @ livein_types) in
	let post_skeletons = binding_skeleton options PostBinding typemap constant_options_map postbind_inputs liveout_types [] in
	(* let _ = Printf.printf "Types are: \n(postbind: %s)\n(prebind: %s)\n" (skeleton_dimension_probabilistic_group_type_list_to_string postbind_inputs) (skeleton_dimension_probabilistic_group_type_list_to_string prebind_inputs) in *)
	(* Flatten the skeletons that had multiple options for dimvars.  *)
	let flattened_pre_skeletons = flatten_skeleton options pre_skeletons in
	let flattened_post_skeletons = flatten_skeleton options post_skeletons in
    (* let () = Printf.printf "Pre skeletons are %s\n" (flat_skeleton_list_to_string flattened_pre_skeletons) in *)
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
    let skeleton_pair_objects = List.map all_skeleton_paris ~f:(fun (((rangecheck, pre), (inputmap, post_check_valid)), post) ->
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
    let sensible_skeleton_pairs = List.filter skeleton_pair_objects ~f:(skeleton_pair_check options apispec) in
	let () = if options.debug_generate_skeletons then
        (Printf.printf "Number of types (livein IO=%d, livein API=%d, liveout API=%d, liveout IO=%d)\n"
            (List.length livein_types) (List.length livein_api_types)
            (List.length liveout_api_types) (List.length liveout_types);
		Printf.printf "For this typemap:\n";
        Printf.printf "Number of pre-bindings generated is %d\n" (List.length pre_skeletons);
        Printf.printf "Number of post-bindings generated is %d\n" (List.length post_skeletons);
		Printf.printf "Number of pre-bindings after filtering is %d\n" (List.length range_checked_pre_skeletons);
		Printf.printf "Number of post-bindings after filtering is %d\n" (List.length range_checked_post_skeletons);
        Printf.printf "Number of skeletons generated is %d\n" (List.length skeleton_pair_objects);
        Printf.printf "Number of skeletons post-filtering is %d\n" (List.length sensible_skeleton_pairs);
		(* Printf.printf "Returnvars requiring a define are %s\n" (String.concat ~sep:", " define_only_return_vars); *)
        )
	else () in
	if options.debug_generate_skeletons then
        (
		Printf.printf "Liveout types are %s\n" (String.concat ~sep:", " (List.map liveout_types ~f:skeleton_dimension_group_type_to_string));
		Printf.printf "Livein types are %s\n" (String.concat ~sep:", " (List.map livein_types ~f:skeleton_dimension_group_type_to_string));
		Printf.printf "Liveout API types are %s\n" (String.concat ~sep:", " (List.map liveout_api_types ~f:skeleton_dimension_group_type_to_string));
		Printf.printf "Livein API types are %s\n" (String.concat ~sep:", " (List.map livein_api_types ~f:skeleton_dimension_group_type_to_string));
        (* Currently no postbinding filtering visible in this function.
        Printf.printf "Number of pre-bindings post-filterings is %d" (List.length 
        Printf.printf "Number of post-bindings post-filterings is %d" (List.length 
        *)
        ())
    else
        ();
	if options.dump_skeletons then
		(Printf.printf "Pre bindings are: %s\n" (String.concat ~sep:"\nNEW PRE SKEL\n" (List.map prebind_inputs ~f:(skeleton_dimension_probabilistic_group_type_to_string)));
		())
	else
		();
	(* Assert that all the pairs are valid structures, e.g. assigning once etc. *)
	let () = verify_skeleton_pairs options typemap iospec apispec sensible_skeleton_pairs in
    sensible_skeleton_pairs

let generate_all_skeleton_pairs opts typemaps iospec apispec =
	let result = List.concat (
		List.map typemaps ~f:(fun typemap ->
            let result = generate_skeleton_pairs opts typemap iospec apispec in
            (* let () = Printf.printf "Keys in typemap are %s%!\n" (String.concat ~sep:"\n" (List.map result ~f:(fun r -> (String.concat ~sep:", " (Hashtbl.keys r.typemap.variable_map))))) in *)
            result
		)
	) in
    let deduplicated =
        deduplicate_skeletons opts result in
	let () = if opts.debug_generate_skeletons then
		Printf.printf "Total number fo skeletons (across all typemaps) is %d\n" (List.length result)
	else ()
	in
    deduplicated
