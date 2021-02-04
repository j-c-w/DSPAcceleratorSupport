open Spec_definition;;
open Spec_utils;;
open Core_kernel;;
open Options;;

(* This module deals with generating synthesis skeletons.
  It uses a number of heuristics to deal with the problems involved
  in doing so.  *)

exception SkeletonGenerationException of string

(* This is an abstracted type that is used
   for matching different likely compatible types
   together.  It is for the signitures only.  *)
(* The idea is to create an abstraction that represents
   values that are of a similar dimension.
   We have the two base types, SInt and SFloat,
   and then a 'dimension' of SArray.
   Other dimensions would be sensible, e.g. maybe
   a pointer type.  *)
(* Ultimately, we might like to include other
   analysis information along with these types. *)
type skeleton_type =
	(* Base types *)
	| SInt of name_reference
	| SFloat of name_reference
and skeleton_dimension_group_type =
	(* Dimension types -- we don't give these names directly.  *)
	| SType of skeleton_type
	| STypes of skeleton_dimension_group_type list
	| SArray of skeleton_dimension_group_type * dimension_type

(* These store bindings.  They are both of the form <from> * <to> *)
type single_variable_binding_option = {
	fromvars: name_reference list;
	tovar: name_reference;
	(* Which dimensions is this assignment valid over? *)
	valid_dimensions: dimension_type
}

(* Storing one binding for every variable.  *)
type skeleton_type_binding = {
	bindings: single_variable_binding_option list
}

let skeleton_type_to_string stype =
	match stype with
	| SInt(name) -> "SInt(" ^ (name_reference_to_string name) ^ ")"
	| SFloat(name) -> "SFloat" ^ (name_reference_to_string name) ^ ")"

let rec skeleton_dimension_group_type_to_string stype =
	match stype with
	| SType(subtype) -> skeleton_type_to_string subtype
	| STypes(subtype) -> "STypes(" ^ (String.concat ~sep:", " (List.map subtype skeleton_dimension_group_type_to_string)) ^ ")"
	| SArray(subdim, lenvar) -> "SArray(" ^ (skeleton_dimension_group_type_to_string subdim) ^
								": with lenvar " ^ (dimension_type_to_string lenvar) ^ ")"

let skeleton_type_binding_to_string skeleton =
	"SKELETON:" ^ String.concat ~sep:"\n" (
	List.map skeleton.bindings (fun binding ->
	"Mapping to " ^ (name_reference_to_string binding.tovar) ^
	   " from vars " ^ (String.concat ~sep:", " (List.map binding.fromvars name_reference_to_string))
	))

let skeleton_dimension_group_types_to_string typs =
    String.concat ~sep:"DimensionType:" (List.map typs skeleton_dimension_group_type_to_string)

let skeleton_list_to_string bindings =
	"BINDINGS"^ String.concat ~sep:" (new var): \n" (
		List.map bindings (fun bindings_for_var -> String.concat ~sep:"\n" (
			List.map bindings_for_var.bindings (fun (bindings) ->
			"From " ^ (String.concat (List.map bindings.fromvars name_reference_to_string)) ^ 
			" to " ^ (name_reference_to_string bindings.tovar)
			)))
	)

let skeletons_to_string skeletons =
	String.concat ~sep:"\n" (List.map skeletons skeleton_type_binding_to_string)

let skeleton_pairs_to_string skeletons =
	String.concat ~sep:"\n" (List.map skeletons (fun (pre, post) ->
		"Pre: " ^ (skeleton_type_binding_to_string pre) ^
		"\n\nPost" ^ (skeleton_type_binding_to_string post)))

let typesets_to_string t =
	String.concat ~sep:") List (" (List.map t (fun t -> (String.concat ~sep:", " (List.map t skeleton_type_to_string))))

let types_to_string t =
	String.concat ~sep:", " (List.map t skeleton_type_to_string)

let name_refs_from_skeleton sk =
	match sk with
	| SInt(nr) -> nr
	| SFloat(nr) -> nr

let rec contains x ys =
	match ys with
	| [] -> false
	| y :: ys -> (x = y) || (contains x ys)

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
	| n1, SInt(n2) -> (n1 = n2)
	| n1, SFloat(n2) -> (n1 = n2)

let dimension_types_to_names d =
	match d with
	| EmptyDimension -> []
	| Dimension(nrs) -> nrs
	(* Don't think I should have to impelemnt this?*)
	| HigherDimention(_, _) -> raise (SkeletonGenerationException "Unimplemented")

let rec flatten_stypes sty = 
	List.concat (List.filter_map sty
	(fun ty -> match ty with
	| SType(x) -> Some([x])
	| STypes(x) -> Some(flatten_stypes x)
	| SArray(_, _) -> None))

let rec variable_in_dim_type var typ =
	match typ with
	| SType(t) -> variable_in_type var t
	| STypes(t) -> List.exists t (variable_in_dim_type var)
	| SArray(t, dt) -> variable_in_dim_type var t

let rec get_dimension_intersections (dim_typesets_in: skeleton_dimension_group_type list) (variables: name_reference list) (original_valid_list: name_reference list) =
	(* Get the right dims for each variable assigned.  *)
	let var_typesets = List.map variables (fun var ->
		(* Get the type of the input dimension type that is
		defined by variables.  *)
		let artype: skeleton_dimension_group_type = 
			match (List.find dim_typesets_in (fun (dim: skeleton_dimension_group_type) ->
			match dim with
			| SArray(artyps, ardims) ->
					if variable_in_dim_type var artyps then
						true
					else
						false
			| _ -> raise (SkeletonGenerationException "Can't look for dimension types in non dimension!")
		)) with
		| Some(v) -> v
		| None -> raise (SkeletonGenerationException "") in
		let found_dims =
			match artype with
			| SArray(_, dims) -> (
					match dims with
					| Dimension(dimnames) -> dimnames
					| _ -> raise (SkeletonGenerationException "Pretty sure that the type assignment phases should only assign single-dimensional names")
			)
			| _ -> raise (SkeletonGenerationException "Not possible")
		in
		found_dims
	) in
	(* Now we have a list of dimension vars, need to compute
	   the intersection.  *)
	big_intersection (original_valid_list :: var_typesets)

let rec prepend_all_strings prep all =
    match all with
    | [] -> []
    | x :: xs -> (prep ^ x) :: (prepend_all_strings prep xs)

let rec prepend_all prep all =
	match all with
	| [] -> []
	| x :: xs -> (prep :: x) :: (prepend_all prep xs)

let rec prepend_all_name_refs prep all =
	match all with
	| [] -> []
	| v :: vs -> (
		match v with
		| Name(_) as other_name -> StructName([prep; other_name])
		| StructName(existing_list) -> StructName(v :: existing_list)
	) :: (prepend_all_name_refs prep vs)

let rec has_overlap x y =
	match x with
	| [] -> false
	| x :: xs -> (contains x y) || (has_overlap xs y)

let rec dimensions_overlap x y =
	match x, y with
	| EmptyDimension, _ -> false
	| _, EmptyDimension -> false
	| Dimension(nrefs), Dimension(nrefs2) -> has_overlap nrefs nrefs2
	| _ -> raise (SkeletonGenerationException "TODO May need to handle")

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
				(fun (memtyp, memname) -> (generate_typesets classmap memtyp memname)) in
			STypes(types))
	| Array(subtyp, lenvar) ->
            let subtyps = generate_typesets classmap subtyp "" in
            (* Array types have no subnames. The subnames get put in the SArray struct so that
			   they can be used when required.  *)
			SArray(subtyps, lenvar)
	| Unit -> raise (SkeletonGenerationException "Can't Unit typesets")
	| Fun(_, _) -> raise (SkeletonGenerationException "Cannot generate typesets from a fun")
	(* Everything else goes to itself.  *)
	| Int16 -> SType(SInt(Name(inpname)))
	| Int32 -> SType(SInt(Name(inpname)))
	| Int64 -> SType(SInt(Name(inpname)))
	| Float16 -> SType(SFloat(Name(inpname)))
	| Float32 -> SType(SFloat(Name(inpname)))
	| Float64 -> SType(SFloat(Name(inpname)))

let skeleton_type_lookup classmap (typemap: (string, synth_type) Hashtbl.t) names =
    List.map names (fun name -> generate_typesets classmap (Hashtbl.find_exn typemap name) name)

let rec prepend_all prep all =
	match all with
	| [] -> []
	| x :: xs -> (prep :: x) :: (prepend_all prep xs)


(* TODO --- We can reduce the number of bindings we need to check here. *)
let binding_check binding = true

(* Determines whether a type from_t is compatible with
   a type to_t --- that is, can we get the information
   to create a suitably-valued to_t with the information
			in a from_t.  *)
let rec compatible_types from_t to_t: bool =
	match from_t, to_t with
	| SInt(nfrom), SInt(nto) as to_t -> true
	| SFloat(nfrom), SFloat(nto) as to_t -> true
	(* There are all kinds of other conversions that could be matched,
	   just need to add them in here.  *)
	| _ -> false

(* This takes two typelists, and removes the compatible variabes
   from each one.
   *)
and intersect_and_remove xtypes ytypes =
	match (xtypes, ytypes) with
	| (xtype :: xtyps, ytype :: ytyps) ->
			let subuniontypes, subxtypes, subytypes =
                intersect_and_remove xtyps ytyps in
			(* Get any sub-bindings that these types might have, e.g. if they
			are modifier types like arrays that have to be compatible 'underneath'
			also.  *)
			let types_are_compatible = compatible_types xtype ytype in
			if types_are_compatible then
                (xtype :: subuniontypes, subxtypes, subytypes)
			else
				(subuniontypes, xtype :: subxtypes, ytype :: subytypes)
	| (xtyps, ytyps) -> ([], xtyps, ytyps)

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
and bindings_for (typesets_in: skeleton_type list) (output: skeleton_type): skeleton_type list list =
	(* Create all reasonable sets of bindings.  *)
	match typesets_in with
	| [] ->
			(* No matches possible from an enpty list of assigning
			   variables! *)
			[]
	| itypeset :: rtypesets_in ->
			(* If there is type overlap, then get that out. *)
			let (intersection_types: skeleton_type list), otypeset_remaining, _ =
                intersect_and_remove [itypeset] [output] in
			(* Try using this variable to assign to the output
			   variable and also try not using it.  *)
			(* With the assignments made here. *)
			let subtask_with_intersection: skeleton_type list list =
                (* The match wasn't complete, so recurse *)
				if otypeset_remaining <> [] then
                    let (subtask_with_intersection: skeleton_type list list) = bindings_for rtypesets_in output in
                    (List.map subtask_with_intersection (fun (target_vars_lists: skeleton_type list): skeleton_type list ->
						(List.concat [intersection_types; target_vars_lists])))
				else
                    (* The match was complete --- there is no point in recursing, this assignment
                       is good endouh. *)
					[intersection_types]
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
and possible_bindings (typesets_in: skeleton_dimension_group_type list) (types_out: skeleton_dimension_group_type list): single_variable_binding_option list list = 
    List.concat (List.map types_out (fun type_out ->
		(*  Make sure that all the possible assingments
		    have the same dimension.  *)
		match type_out with
		(* If this is a dimension type, we need to recurse
		   into the subtypes, which might also be dimension types.
		   (And then we need to find appropriate assignments for
		   those). *)
		| SArray(array_subtyps, dim_options) ->
				let valid_dimensioned_typesets_in: skeleton_dimension_group_type list = List.filter typesets_in (fun intype ->
					match intype with
					| SArray(_, in_dim_options) ->
							(* If there is any overlap in the possible typesets,
							   include this.  *)
							dimensions_overlap dim_options in_dim_options
					(* non-dimension types can't be included. *)
					| _ -> false
				) in
				(* Get the STypes from these arrays, but keep the
				   dim_options.  *)
				let array_stypes: skeleton_dimension_group_type list =
						List.map valid_dimensioned_typesets_in (fun intype ->
							match intype with
							| SArray(artyp, dims) -> artyp
							| _ -> raise (SkeletonGenerationException "")
						) in
				(* Recurse for the matches.  *)
				let recurse_assignments: single_variable_binding_option list list = possible_bindings valid_dimensioned_typesets_in array_stypes in
				(* Need to put these stypes into the arrays
				   they were just taken out of.  *)
				List.map recurse_assignments (fun assign_list ->
				List.map assign_list (fun assignments ->
					(* Get the varaibles that would make
					a valid subdimension.  This is probably
					quite a slow operation as it computes
					the n-way intersection.  *)
					let dim_option_names = dimension_types_to_names dim_options in
					let valid_dimension_vars = get_dimension_intersections valid_dimensioned_typesets_in assignments.fromvars dim_option_names in
					let sub_dimensions = assignments.valid_dimensions in
					(* Add the subdimensions we just computed
					to any calcualted subdimensions.  *)
					let injected_sub_dimensions =
						match sub_dimensions with
						| EmptyDimension -> Dimension(valid_dimension_vars)
						| higher_dimension ->
								HigherDimention(higher_dimension, valid_dimension_vars) in
					{
						fromvars = assignments.fromvars;
						tovar = assignments.tovar;
						valid_dimensions = injected_sub_dimensions
					}
				))
		(* If this isn't an array, then allow any type
		   that isn't an array. *)
		| STypes(_) -> raise (SkeletonGenerationException "Need to flatten STypes out before getting mappings")
		| SType(stype_out) ->
				let zero_dimtypes = flatten_stypes typesets_in in
				let allbindings = bindings_for zero_dimtypes stype_out in
        (* The bindings_for doesn't create the whole skeleton_type_binding or
           name_binding types.  This needs to add the informtion about
           the out variable to each set to achieve that.  *)
		[List.map allbindings (fun binding ->
			{fromvars = List.map binding name_refs_from_skeleton; tovar = name_refs_from_skeleton stype_out; valid_dimensions = EmptyDimension})]
		))

(* Given a list of variables, and an equally sized list of
   possible bindings, select one of the possible bindings
   for each variable.  *)
(* For now we try to create all possible bindings and filter them
   later.   May need a more efficient search strategy eventually.  *)
let rec find_possible_skeletons (possible_bindings_for_var: single_variable_binding_option list list): skeleton_type_binding list =
    (* TODO --- Fix this function so it isn't empty *)
	match (possible_bindings_for_var) with
    | [] -> [] (* No bindings needed *)
	(* Create the basis for the binding sets we are gong to explore *)
    | binding_list :: [] -> List.map binding_list (fun b -> {
		bindings = [b]
	})
	| binding :: bindings ->
			let subbindings = find_possible_skeletons bindings in
			(* Include each possible set of bindings for this variable. *)
            (* Note that we can't just /not/ bind a variable, so we need to
               make sure that we don't skip a binding at any
               occasion.  *)
			List.concat (List.map binding (fun b -> List.map subbindings (fun sub -> { bindings = (b :: sub.bindings)})))

let possible_skeletons possible_bindings_for_var =
    find_possible_skeletons possible_bindings_for_var

(* TODO --- Some filtering here might be a good idea.  *)
let skeleton_check skel = true

(* TODO --- Some filtering here would be a good idea.  *)
let skeleton_pair_check p = true

(* The algorithm should produce bindings from the inputs
   the outputs. *)
let binding_skeleton options classmap typesets_in typesets_out =
	(* Get a list of list of possible inputs for each
	   output.  This is type filtered, so the idea
	   is that it is sane.  *)
	let possible_bindings_list: single_variable_binding_option list list = possible_bindings typesets_in typesets_out in
	(* Then, filter out various bindings that might not make any sense.  *)
	let sensible_bindings = List.filter possible_bindings_list binding_check in
	(* Finally, create some skeletons from those bindings.  *)
    (* Need to have one set of bindings for each output variable.  *)
	let possible_skeletons_list: skeleton_type_binding list = possible_skeletons sensible_bindings in
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
let generate_skeleton_pairs options (classmap: (string, structure_metadata) Hashtbl.t) (iospec: iospec) (apispec: apispec): (skeleton_type_binding * skeleton_type_binding) list =
    (* Get the types of the varios input variables.  *)
    let livein_types = skeleton_type_lookup classmap iospec.typemap iospec.livein in
    let livein_api_types = skeleton_type_lookup classmap apispec.typemap apispec.livein in
    let liveout_api_types = skeleton_type_lookup classmap apispec.typemap apispec.liveout in
    let liveout_types = skeleton_type_lookup classmap iospec.typemap iospec.liveout in
    (* Now use these to create skeletons.  *)
	let pre_skeletons: skeleton_type_binding list = binding_skeleton options classmap livein_types livein_api_types in
    let post_skeletons = binding_skeleton options classmap liveout_api_types liveout_types in
    (* Do skeleton pairing *)
    let all_skeleton_paris = List.cartesian_product pre_skeletons post_skeletons in
    (* Do joint filtering *)
    let sensible_skeleton_pairs = List.filter all_skeleton_paris skeleton_pair_check in
	if options.debug_generate_skeletons then
        (Printf.printf "Number of types (livein IO=%d, livein API=%d, liveout API=%d, liveout IO=%d)\n"
            (List.length livein_types) (List.length livein_api_types)
            (List.length liveout_api_types) (List.length liveout_types);
		Printf.printf "Liveout types are %s\n" (String.concat ~sep:", " (List.map liveout_types skeleton_dimension_group_type_to_string));
		Printf.printf "Livein types are %s\n" (String.concat ~sep:", " (List.map livein_types skeleton_dimension_group_type_to_string));
		Printf.printf "Liveout API types are %s\n" (String.concat ~sep:", " (List.map liveout_api_types skeleton_dimension_group_type_to_string));
		Printf.printf "Livein API types are %s\n" (String.concat ~sep:", " (List.map liveout_api_types skeleton_dimension_group_type_to_string));
        Printf.printf "Number of pre-bindings generated is %d\n" (List.length pre_skeletons);
        Printf.printf "Number of post-bindings generated is %d\n" (List.length post_skeletons);
        (* Currently no postbinding filtering visible in this function.
        Printf.printf "Number of pre-bindings post-filterings is %d" (List.length 
        Printf.printf "Number of post-bindings post-filterings is %d" (List.length 
        *)
        Printf.printf "Number of skeletons generated is %d\n" (List.length all_skeleton_paris);
        Printf.printf "Number of skeletons post-filtering is %d\n" (List.length sensible_skeleton_pairs);
        ())
    else
        ();
    sensible_skeleton_pairs
