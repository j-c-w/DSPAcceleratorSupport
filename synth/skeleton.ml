open Spec_definition;;
open Spec_utils;;
open Core_kernel;;
open Options;;

(* This module deals with generating synthesis skeletons.
  It uses a number of heuristics to deal with the problems involved
  in doing so.  *)

exception SkeletonGenerationException of string

(* We need to have recursive types to store the name mappings,
   since that's what the type inputs that we take can be.  *)
type name_reference =
	| Name of string
	(* To represent class names and member variables.  This
	   DOES NOT mean the list of all members of a class, but
	   rather the list of member names you have to traverse
	   to get to the member. *)
	| StructName of name_reference list

(* This is an abstracted type that is used
   for matching different likely compatible types
   together.  It is for the signitures only.  *)
type skeleton_type =
	| SInt
	| SFloat
	| SArray of skeleton_type
	| SStruct of skeleton_type list * name_reference list

(* These store bindings.  They are both of the form <from> * <to> *)
type name_binding = (name_reference * name_reference)
type skeleton_type_binding = (skeleton_type * skeleton_type)

(* Stores a list of skeletons, i.e. variable bindings. *)
type skeleton = {
    bindings: name_binding list;
    binding_types: skeleton_type_binding list
}

let rec skeleton_type_to_string stype =
	match stype with
	| SInt -> "SInt"
	| SFloat -> "SFloat"
	| SArray(subtype) -> "SArray(" ^ (skeleton_type_to_string subtype) ^ ")"
	| SStruct(subtypes, subnames) -> "SStruct(" ^ (String.concat ~sep:", " (List.map subtypes skeleton_type_to_string)) ^ ")"

let rec name_reference_to_string nref =
	match nref with
	| Name(s) -> s
	| StructName(ns) -> (String.concat ~sep:"." (List.map ns name_reference_to_string))

let skeleton_to_string skeleton =
	"SKELETON:" ^ String.concat ~sep:"\n" (
	List.map skeleton.bindings (fun binding -> match binding with
	| (tovar, fromvars) -> "Mapping to " ^ (name_reference_to_string tovar) ^
						   " from vars " ^ (name_reference_to_string fromvars)
	))

let binding_sets_to_string bindings =
	"BINDINGS"^ String.concat ~sep:" (new var): \n" (
		List.map bindings (fun bindings_for_var -> String.concat ~sep:"\n" (
			List.map bindings_for_var (fun ((from_names, to_names), (from_types, to_types)) ->
			"From " ^ (String.concat (List.map from_names name_reference_to_string)) ^ " (with types " ^
				(skeleton_type_to_string from_types) ^
			") to " ^ (String.concat (List.map to_names name_reference_to_string)) ^   " (with types " ^
				(skeleton_type_to_string to_types)))))

let skeletons_to_string skeletons =
	String.concat ~sep:"\n" (List.map skeletons skeleton_to_string)

let skeleton_pairs_to_string skeletons =
	String.concat ~sep:"\n" (List.map skeletons (fun (pre, post) -> "Pre: " ^ (skeleton_to_string pre) ^ "\n\nPost" ^ (skeleton_to_string post)))

let typesets_to_string t =
	String.concat ~sep:") List (" (List.map t (fun t -> (String.concat ~sep:", " (List.map t skeleton_type_to_string))))

let type_lookup map names =
    List.map names (fun name -> Hashtbl.find_exn map name)

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

(* Generates the base typesets for a class. *)
let rec generate_typesets classmap inptype inpname =
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
            let types, names = List.unzip (List.map subtypes (fun (memtyp, memname) -> (generate_typesets classmap memtyp memname))) in
            (SStruct(types, names), Name(name))
			)
	| Array(subtyp) ->
            let subtyps, subnames = generate_typesets classmap subtyp "" in
            (* Array types have no subnames. The subnames get put in the SArray struct so that
			   they can be used when required.  *)
            (SArray(subtyps), Name(inpname))
	| Unit -> raise (SkeletonGenerationException "Can't Unit typesets")
	| Fun(_, _) -> raise (SkeletonGenerationException "Cannot generate typesets from a fun")
	(* Everything else goes to itself.  *)
	| Int16 -> (SInt, Name(inpname))
    | Int32 -> (SInt, Name(inpname))
    | Int64 -> (SInt, Name(inpname))
    | Float16 -> (SFloat, Name(inpname))
    | Float32 -> (SFloat, Name(inpname))
    | Float64 -> (SFloat, Name(inpname))

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
let rec compatible_types from_t to_t: ((string list * skeleton_type list) list * bool) =
	match from_t, to_t with
	| SInt, SInt -> [], true
	| SFloat, SFloat -> [], true
	| SArray(from_t), SArray(to_t) ->
			(* Call with 'fake' names because the arrays don't have binding names (except
			for those used at the top level).  Anyway, we need this binding to happen. *)
			let resbindings = bindings_for ([Name("INTERNAL_NAME_DONT_USE")], [from_t]) ([Name("INTERNAL_NAME_DONT_USE")], [to_t]) in
			let filtered_resbindings = List.filter resbindings binding_check in
			let result = (List.for_all filtered_resbindings (fun (bind, _) -> bind <> [])) in
			if result = true then
				filtered_resbindings, result
			else
				[], result
	| SStruct(from_ts, from_names), SStruct(to_ts, to_names) ->
			(* We may need to memoize this case, as it could conceivable
			   get called a lot with very similar arguments.  *)
			(* Need to check that there is some sensible variable
			   assignment here.  *)
			let resbindings = bindings_for (from_names, from_ts) (to_names, to_ts) in
			(* Filter to avoid useless bindings.  *)
			let filtered_resbindings = List.filter resbindings binding_check in
			(* None of the bindings can be empty --- if they are then the
			   calculation failed.  *)
			let result = (List.for_all filtered_resbindings (fun (bind, _) -> bind <> [])) in
			if result = true then
				filtered_resbindings, result
			else
				[], result
	(* There are all kinds of other conversions that could be matched,
	   just need to add them in here.  *)
	| _ -> [], false

(* This takes two typelists, and removes the compatible variabes
   from each one.
   *)
and intersect_and_remove (xnames, xtypes) (ynames, ytypes) =
	match (xnames, xtypes, ynames, ytypes) with
	| (xname :: xnms, xtype :: xtyps, yname :: ynms, ytype :: ytyps) ->
			let subunionnames, subuniontypes, bindings, subxnames, subxtypes, subynames, subytypes =
                intersect_and_remove (xnms, xtyps) (ynms, ytyps) in
			(* Get any sub-bindings that these types might have, e.g. if they
			are modifier types like arrays that have to be compatible 'underneath'
			also.  *)
			let subbindings, types_are_compatible = compatible_types(xtype, ytype) in
			if types_are_compatible then
                (xname :: subunionnames, xtype :: subuniontypes, subbindings :: bindings, subxnames, subxtypes, subynames, subytypes)
			else
				(subunionnames, subuniontypes, bindings, xname :: subunionnames, xtype :: subxtypes, yname :: subynames, ytype :: subytypes)
	| (xnms, xtyps, ynms, ytyps) -> ([], [], [], xnms, xtyps, ynms, ytyps)

(* Every output has to have either an input assigned to it,
   or no inputs assigned (in which case a constant can be
   used).  Generate all the mappings for one particular output. *)
(* Since this should be binding a single output variable, it
   does not produce a null name_binding, as the name of the output
   variable will be bound once the function ends.  Similarly
   for the skeleton_type_binding.  *)
and bindings_for ((inputs, typesets_in): string list list * skeleton_type list list) (output, typeset_out): (string list * skeleton_type list) list =
	(* Create all reasonable sets of bindings.  *)
	match (inputs, typesets_in) with
	| ([], []) ->
            if typeset_out <> [] then
                (* No assignments possible. *)
                []
            else
                (* No assignments required to assign to all of typeset_out, so just
                   return that! *)
                [([], [])]
	| (ivar :: rinputs, itypeset :: rtypesets_in) ->
			(* If there is type overlap, then get that out. *)
			let intersection_names, (intersection_types: skeleton_type list), intersection_bindings, onameset_remaining, otypeset_remaining, _, _ =
                intersect_and_remove (ivar, itypeset) (output, typeset_out) in
			(* Try using this variable to assign to the output
			   variable and also try not using it.  *)
			(* With the assignments made here. *)
			let subtask_with_intersection: (name_reference list * skeleton_type list) list =
                (* The match wasn't complete, so recurse *)
				if otypeset_remaining <> [] then
                    let subtask_with_intersection = bindings_for (rinputs, rtypesets_in) (onameset_remaining, otypeset_remaining) in
                    List.map subtask_with_intersection (fun (names, target_vars_lists) ->
                        (List.concat [intersection_names; names], intersection_types @ target_vars_lists))
				else
                    (* The match was complete --- there is no point in recursing, this assignment
                       is good endouh. *)
					Need some way to put any intersection assignments in here.
                    [(intersection_names, intersection_types)]
			in
			(* Without the assignments made here. *)
			let subtask_without_intersection =
				bindings_for (rinputs, rtypesets_in) (output, typeset_out)
			in
			subtask_with_intersection @ subtask_without_intersection
	| _ -> raise (SkeletonGenerationException "ivar inputs and typesets must be equal lengths")


(* This is the difficult one.  It is hard to tell
   what the optimum set of bindings is, although
   it likely includes fairly low binding reuse.

   Need to get a set of assignments for each
   output. *)
and possible_bindings (inputs, typesets_in) (outputs, typesets_out) = 
    List.map (List.zip_exn outputs typesets_out) (fun (out, typeset) ->
        let allpairs = bindings_for (inputs, typesets_in) (out, typeset) in
        (* The bindings_for doesn't create the whole skeleton_type_binding or
           name_binding types.  This needs to add the informtion about
           the out variable to each set to achieve that.  *)
        List.map allpairs (fun (vnames, vtypes) -> (
            (out, vnames), (* name binding *)
            (typeset, vtypes) (* type binding *)
        )))

(* Given a list of variables, and an equally sized list of
   possible bindings, select one of the possible bindings
   for each variable.  *)
(* For now we try to create all possible bindings and filter them
   later.   May need a more efficient search strategy eventually.  *)
let rec find_possible_skeletons ivariables (possible_bindings_for_var: (name_binding * skeleton_type_binding) list list) =
    (* TODO --- Fix this function so it isn't empty *)
	match (ivariables, possible_bindings_for_var) with
    | [], [] -> [] (* No bindings needed *)
	(* Create the basis for the binding sets we are gong to explore *)
    | var :: [], binding_list :: [] -> List.map binding_list (fun b -> [b])
	| var :: vars, binding :: bindings -> 
			let subbindings = find_possible_skeletons vars bindings in
			(* Include each possible set of bindings for this variable. *)
            (* Note that we can't just /not/ bind a variable, so we need to
               make sure that we don't skip a binding at any
               occasion.  *)
			List.concat (List.map binding (fun b -> prepend_all b subbindings))
	| _ -> raise (SkeletonGenerationException "Each ivariable must have a bindings set")

let possible_skeletons ivariables possible_bindings_for_var =
    List.map (List.map (find_possible_skeletons ivariables possible_bindings_for_var) List.unzip)
    (fun (n_binds, t_binds) -> { bindings=n_binds; binding_types=t_binds } )

(* TODO --- Some filtering here might be a good idea.  *)
let skeleton_check skel = true

(* TODO --- Some filtering here would be a good idea.  *)
let skeleton_pair_check p = true

(* The algorithm should produce bindings from the inputs
   the outputs. *)
let binding_skeleton options classmap (input_vars, input_types) (output_vars, output_types) =
	(* Get a list of list of possible inputs for each
	   output.  This is type filtered, so the idea
	   is that it is sane.  *)
	let typesets_in, (varlists_in: string list list) = List.unzip (List.map (List.zip_exn input_types input_vars) (fun (typ, nam) -> generate_typesets classmap typ nam)) in
	let typesets_out, varlists_out = List.unzip (List.map (List.zip_exn output_types output_vars) (fun (typ, nam) -> generate_typesets classmap typ nam)) in
	let possible_bindings_list: (name_binding * skeleton_type_binding) list list = possible_bindings (varlists_in, typesets_in) (varlists_out, typesets_out) in
	(* Then, filter out various bindings that might not make any sense.  *)
	let sensible_bindings = List.filter possible_bindings_list binding_check in
	(* Finally, create some skeletons from those bindings.  *)
    (* Need to have one set of bindings for each output variable.  *)
    assert ((List.length output_vars) = (List.length sensible_bindings));
	let possible_skeletons_list: skeleton list = possible_skeletons output_vars sensible_bindings in
	let sensible_skeletons = List.filter possible_skeletons_list skeleton_check in
	(* Debug info: *)
	if options.debug_generate_skeletons then
		(Printf.printf "Call to binding_skeleton\n";
		Printf.printf "Typesets in is %s\n" (typesets_to_string typesets_in);
		Printf.printf "Typesets out is %s\n" (typesets_to_string typesets_out);
		Printf.printf "Number of possible bindings is %d\n" (List.length possible_bindings_list);
		Printf.printf "Number of sensible bindings is %d\n" (List.length sensible_bindings);
		Printf.printf "Sensible Bindings are: %s\n" (binding_sets_to_string sensible_bindings);
		Printf.printf "Number of possible skeletons is %d\n" (List.length possible_skeletons_list);
		Printf.printf "Number of sensible skeletons is %d\n" (List.length sensible_skeletons);
		()
		
		)
	else
		();
	sensible_skeletons

(* Given the input classmap, IOSpec, and APISpec, generate
   the pre- and post-skeletons.  Pair them, and do some
   filtering to check for sanity.  *)
let generate_skeleton_pairs options classmap (iospec: iospec) (apispec: apispec) =
    (* Get the types of the varios input variables.  *)
    let livein_types = type_lookup iospec.typemap iospec.livein in
    let livein_api_types = type_lookup apispec.typemap apispec.livein in
    let liveout_api_types = type_lookup apispec.typemap apispec.liveout in
    let liveout_types = type_lookup iospec.typemap iospec.liveout in
    (* Now use these to create skeletons.  *)
	let pre_skeletons: skeleton list = binding_skeleton options classmap (iospec.livein, livein_types) (apispec.livein, livein_api_types) in
    let post_skeletons = binding_skeleton options classmap (apispec.liveout, liveout_api_types) (iospec.liveout, liveout_types) in
    (* Do skeleton pairing *)
    let all_skeleton_paris = List.cartesian_product pre_skeletons post_skeletons in
    (* Do joint filtering *)
    let sensible_skeleton_pairs = List.filter all_skeleton_paris skeleton_pair_check in
	if options.debug_generate_skeletons then
        (Printf.printf "Number of types (livein IO=%d, livein API=%d, liveout API=%d, liveout IO=%d)\n"
            (List.length livein_types) (List.length livein_api_types)
            (List.length liveout_api_types) (List.length liveout_types);
		Printf.printf "Liveout types are %s\n" (String.concat ~sep:", " (List.map liveout_types synth_type_to_string));
		Printf.printf "Livein types are %s\n" (String.concat ~sep:", " (List.map livein_types synth_type_to_string));
		Printf.printf "Liveout API types are %s\n" (String.concat ~sep:", " (List.map liveout_api_types synth_type_to_string));
		Printf.printf "Livein API types are %s\n" (String.concat ~sep:", " (List.map liveout_api_types synth_type_to_string));
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
