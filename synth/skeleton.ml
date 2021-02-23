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
    (* Except for this one :) because it's really a type and has a name *)
	| SArray of name_reference * skeleton_dimension_group_type * dimension_type

(* Keep track of dimvar mappings required for
   certain loops.  We envision that this will
   eventually include more complicated mappings
   than the direct mappings entailed by this.  *)
type one_dim_var_mapping =
	| ExactVarMatch of name_reference * name_reference

type dimvar_mapping =
    | DimvarOneDimension of one_dim_var_mapping

(* These store bindings.  They are both of the form <from> * <to> *)
type single_variable_binding_option = {
    (* What parts of the names apply to each fromvar? *)
    (* ie. this is trying to keep track of where the list
       index should go, e.g. complexes[i].real vs complexes.real[i].

       One list for each fromvars
       *)
    fromvars_index_nesting: name_reference list list;
    tovar_index_nesting: name_reference list;
	(* Which dimensions is this assignment valid over? *)
	valid_dimensions: dimvar_mapping list
}

(* Storing one binding for every variable.  *)
type skeleton_type_binding = {
	bindings: single_variable_binding_option list
}

let one_dim_var_mapping_to_string map =
	match map with
	| ExactVarMatch(fromv, tov) -> (name_reference_to_string fromv) ^
    " = " ^ (name_reference_to_string tov)

let rec dimvar_mapping_to_string mapping = match mapping with
	| DimvarOneDimension(map) -> one_dim_var_mapping_to_string map

let dimvar_mapping_list_to_string mapping =
    String.concat ~sep:"\n" (List.map mapping dimvar_mapping_to_string)

let skeleton_type_to_string stype =
	match stype with
	| SInt(name) -> "SInt(" ^ (name_reference_to_string name) ^ ")"
	| SFloat(name) -> "SFloat(" ^ (name_reference_to_string name) ^ ")"

let rec skeleton_dimension_group_type_to_string stype =
	match stype with
	| SType(subtype) -> skeleton_type_to_string subtype
	| STypes(subtype) -> "STypes(" ^ (String.concat ~sep:", " (List.map subtype skeleton_dimension_group_type_to_string)) ^ ")"
	| SArray(name, subdim, lenvar) -> "SArray(" ^ (name_reference_to_string name) ^ ": "
                                ^ (skeleton_dimension_group_type_to_string subdim) ^
								": with lenvar " ^ (dimension_type_to_string lenvar) ^ ")"

let skeleton_dimension_group_type_list_to_string typs =
	String.concat ~sep:", " (List.map typs skeleton_dimension_group_type_to_string)

let single_variable_binding_to_string binding =
	   "\nWith the array index wrappers " ^ (String.concat ~sep:"," (List.map binding.tovar_index_nesting name_reference_to_string)) ^
	   "\nAnd (fromvars) [" ^ (String.concat ~sep:"], [" 
		   (List.map binding.fromvars_index_nesting (fun x -> (String.concat ~sep:" ," (List.map x name_reference_to_string))))) ^ "]"

let skeleton_type_binding_to_string skeleton =
	"SKELETON:\n" ^ String.concat ~sep:"\n" (
	List.map skeleton.bindings single_variable_binding_to_string)

let skeleton_dimension_group_types_to_string typs =
    String.concat ~sep:", DimensionType:" (List.map typs skeleton_dimension_group_type_to_string)

let skeleton_list_to_string bindings =
	"BINDINGS:\n"^ (String.concat ~sep:">(new binding): \n" (
		List.map bindings (fun bindings_for_var ->
			skeleton_type_binding_to_string bindings_for_var
	))) ^ "\n"

let double_binding_options_list_to_string opts =
	skeleton_list_to_string (List.map opts (fun o -> { bindings = o }))

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

let rec flatten_stypes sty = 
	List.concat (List.filter_map sty
	(fun ty -> match ty with
	| SType(x) -> Some([x])
	| STypes(x) -> Some(flatten_stypes x)
	| SArray(_, _, _) -> None))

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
	DimvarOneDimension(ExactVarMatch(x, y))

let rec dimvar_contains x y =
	match y with
	| [] -> []
	| (y :: ys) ->
			(dimvar_match x y) :: (dimvar_contains x ys)

let rec has_overlap x y =
	List.concat (List.map x (fun xel -> dimvar_contains xel y ))

let rec dimensions_overlap x y =
	match x, y with
	| EmptyDimension, _ -> []
	| _, EmptyDimension -> []
	| Dimension(nrefs), Dimension(nrefs2) -> has_overlap nrefs nrefs2

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
	| Unit -> raise (SkeletonGenerationException "Can't Unit typesets")
	| Fun(_, _) -> raise (SkeletonGenerationException "Cannot generate typesets from a fun")
	(* Everything else goes to itself.  *)
	| Int16 -> SType(SInt(name_from_opt inpname))
	| Int32 -> SType(SInt(name_from_opt inpname))
	| Int64 -> SType(SInt(name_from_opt inpname))
	| Float16 -> SType(SFloat(name_from_opt inpname))
	| Float32 -> SType(SFloat(name_from_opt inpname))
	| Float64 -> SType(SFloat(name_from_opt inpname))

let skeleton_type_lookup classmap (typemap: (string, synth_type) Hashtbl.t) names =
    List.map names (fun name -> generate_typesets classmap (Hashtbl.find_exn typemap name) (Some(name)))

let rec prepend_all prep all =
	match all with
	| [] -> []
	| x :: xs -> (prep :: x) :: (prepend_all prep xs)

let rec repeat n item =
    match n with
    | 0 -> []
    | n -> item :: (repeat (n - 1) item)


(* TODO --- We can reduce the number of bindings we need to check here. *)
let binding_check binding = true

(* Determines whether a type from_t is compatible with
   a type to_t --- that is, can we get the information
   to create a suitably-valued to_t with the information
			in a from_t.  *)
let rec compatible_types from_t to_t: bool =
	match from_t, to_t with
	| SInt(nfrom), SInt(nto) -> true
	| SFloat(nfrom), SFloat(nto) -> true
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
and possible_bindings options (typesets_in: skeleton_dimension_group_type list) (types_out: skeleton_dimension_group_type list): single_variable_binding_option list list = 
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
							Some(intype, dimensions_overlap dim_options in_dim_options)
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
				(* Recurse for the matches.  *)
				let recurse_assignments: single_variable_binding_option list list =
					(* Put the undimensioned typelists back with their types.  *)
					List.map (List.zip_exn flattened_undimensioned_typesets dim_mappings) (fun ((arrnam, flattened_undimensioned_typeset), dim_mapping) ->
						(* Get the possible bindings.  *)
						let poss_bindings: single_variable_binding_option list list =
							possible_bindings options flattened_undimensioned_typeset flattened_arr_stypes in
						(* Now add the required dimension mappings.  *)
						List.concat(
						List.map poss_bindings (fun var_binds ->
							List.map var_binds (fun bind ->
								{
                                    fromvars_index_nesting = prepend_all arrnam bind.fromvars_index_nesting;
                                    tovar_index_nesting = sarray_nam :: bind.tovar_index_nesting;
                                    valid_dimensions = dim_mapping
                                }
                            )
                        )
                        )
                    ) in
				let () = assert((List.length recurse_assignments) = (List.length valid_dimensioned_typesets_in)) in
				let () = if options.debug_generate_skeletons then
					let () = Printf.printf "ARRAY_ASSIGN: For the flattened subtypes %s\n" (skeleton_dimension_group_type_list_to_string flattened_arr_stypes) in
					let () = Printf.printf "Found the following assignments %s\n" (double_binding_options_list_to_string recurse_assignments) in
					let () = Printf.printf "Had the following types available to find these assignments %s\n" (skeleton_dimension_group_type_list_to_string typesets_in) in
					Printf.printf "Filtered these down to %s\n" (skeleton_dimension_group_type_list_to_string (List.map valid_undimensioned_typesets_in (fun (a, b) -> b)))
				else
					() in
				recurse_assignments
		(* If this isn't an array, then allow any type
		   that isn't an array. *)
		| STypes(_) -> raise (SkeletonGenerationException "Need to flatten STypes out before getting mappings")
		| SType(stype_out) ->
				let zero_dimtypes = flatten_stypes typesets_in in
				let allbindings = bindings_for zero_dimtypes stype_out in
				let () = if options.debug_generate_skeletons then
					Printf.printf "SINGLE_ASSIGN: Assiging to variable '%s' using bindings %s\n" (name_reference_to_string (name_refs_from_skeleton stype_out))
						((skeleton_dimension_group_type_list_to_string typesets_in ))
				else () in
        (* The bindings_for doesn't create the whole skeleton_type_binding or
           name_binding types.  This needs to add the informtion about
           the out variable to each set to achieve that.  *)
		[List.map allbindings (fun binding ->
			{
				fromvars_index_nesting = List.map binding (fun b -> [name_refs_from_skeleton b]);
                tovar_index_nesting = [name_refs_from_skeleton stype_out];
                valid_dimensions = [];
            })]
		))

(* Given a list of variables, and an equally sized list of
   possible bindings, select one of the possible bindings
   for each variable.  *)
(* For now we try to create all possible bindings and filter them
   later.   May need a more efficient search strategy eventually.  *)
let rec find_possible_skeletons options (possible_bindings_for_var: single_variable_binding_option list list): skeleton_type_binding list =
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

let possible_skeletons options possible_bindings_for_var =
    find_possible_skeletons options possible_bindings_for_var

(* TODO --- Some filtering here might be a good idea.  *)
let skeleton_check skel = true

(* TODO --- Some filtering here would be a good idea.  *)
let skeleton_pair_check p = true

(* The algorithm should produce bindings from the inputs
   the outputs. *)
let binding_skeleton options classmap typesets_in typesets_out =
	(* Need to flatten the output typesets to make sure we only
	   look for one type at a type --- this takes the STypes([...])
	   and converts it to [...] *)
	let flattened_typesets_out = List.concat (List.map typesets_out flatten_stype_list) in
	(* Get a list of list of possible inputs for each
	   output.  This is type filtered, so the idea
	   is that it is sane.  *)
	let possible_bindings_list: single_variable_binding_option list list = possible_bindings options typesets_in flattened_typesets_out in
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
	let () = if options.print_synthesizer_numbers || options.debug_generate_skeletons then
        (Printf.printf "Number of types (livein IO=%d, livein API=%d, liveout API=%d, liveout IO=%d)\n"
            (List.length livein_types) (List.length livein_api_types)
            (List.length liveout_api_types) (List.length liveout_types);
        Printf.printf "Number of pre-bindings generated is %d\n" (List.length pre_skeletons);
        Printf.printf "Number of post-bindings generated is %d\n" (List.length post_skeletons);
        Printf.printf "Number of skeletons generated is %d\n" (List.length all_skeleton_paris);
        Printf.printf "Number of skeletons post-filtering is %d\n" (List.length sensible_skeleton_pairs);
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
    sensible_skeleton_pairs
