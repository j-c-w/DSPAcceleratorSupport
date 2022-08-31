open Skeleton_definition;;
open Spec_definition;;
open Spec_utils;;
open Core_kernel;;
open Gir_utils;;
open Builtin_conversion_functions;;

let one_dim_var_mapping_to_string map =
	match map with
	| VarMatch(matches) ->
            String.concat ~sep:"&&" (List.map matches (fun (fromv, tov, mode) ->
            (name_reference_to_string fromv) ^
    " = " ^ (name_reference_to_string tov) ^ (dim_relation_to_string mode)
            ))
	| ConstantMatch(from_const) ->
			(string_of_int from_const)

let rec dimvar_mapping_to_string mapping = match mapping with
	| DimvarOneDimension(map) -> one_dim_var_mapping_to_string map

let dimvar_mapping_list_to_string mapping =
    String.concat ~sep:"\n" (List.map mapping dimvar_mapping_to_string)

let dimension_constraint_to_string cons =
	match cons with
	| DimensionConstraints(dim_map, dimension_value) ->
            "Dimension: " ^ 
            (dimension_value_to_string dimension_value) ^
            " with constraints " ^
            (dimvar_mapping_to_string dim_map)

let dimension_constraint_list_to_string conslist =
    String.concat ~sep:", " (List.map conslist dimension_constraint_to_string)

let one_dimension_mapping_equal m1 m2 =
    match m1, m2 with
    | VarMatch(matches1), VarMatch(matches2) ->
            (
            match (List.zip matches1 matches2) with
            (* All sub-constraints must be equal for these to be equivalent --- we could (and probably should) consider
             * that && is reflexive. *)
            | Ok(l) -> List.for_all l (fun ((fromv1, tov1, mode1), (fromv2, tov2, mode2)) ->
                (name_reference_equal fromv1 fromv2) &&
                (name_reference_equal tov1 tov2) &&
                (dim_relation_equal mode1 mode2)
            )
            | Unequal_lengths -> false
            )
	| ConstantMatch(fconst1), ConstantMatch(fconst2) ->
			(fconst1 = fconst2)
    | _, _ -> false

(* I think this means relexive?  *)
(* Anyway, I think the point is that x = y and y = x are the same --- it's to compare
 * across both pre- and post- bindings, which assign
 * in different orders.
 * It DOES NOT CONSIDER the relfexivity of the && operator.  *)
let one_dimension_mapping_equal_commutative equivalence_map m1 m2 =
    let get_equivalent_variables map v =
        let equivalents = Hashtbl.find map (name_reference_to_string v) in
        match equivalents with
        | Some(vs) -> (name_reference_to_string v) :: vs
        | None -> [(name_reference_to_string v)]
    in
	match m1, m2 with
    | VarMatch(ms1), VarMatch(ms2) ->
            (
            match (List.zip ms1 ms2) with
            (* TODO -- we should really support a = b && c = d and c = d && a = b being equal *)
            | Ok(l) -> List.for_all l (fun ((fromv1, tov1, rel1), (fromv2, tov2, rel2)) -> 
                    match rel1, rel2 with
                    | DimEqualityRelation, DimEqualityRelation ->
                            let from1alternatives = get_equivalent_variables equivalence_map fromv1 in
                            let from2alternatives = get_equivalent_variables equivalence_map fromv2 in
                            let to1alternatives = get_equivalent_variables equivalence_map tov1 in
                            let to2alternatives = get_equivalent_variables equivalence_map tov2 in
                            ((Utils.strings_any_equal from1alternatives from2alternatives) &&
             (Utils.strings_any_equal to1alternatives to2alternatives)) ||
            ((Utils.strings_any_equal from1alternatives to2alternatives) &&
             (Utils.strings_any_equal from2alternatives to1alternatives))
                    (* TODO --- something for pow2? *)
                    | _, _ -> one_dimension_mapping_equal m1 m2
                    )
            | Unequal_lengths -> false
            )
	| other1, other2 ->
			(* No difference for constant matching *)
			one_dimension_mapping_equal other1 other2

let dimvar_equal m1 m2 =
	match m1, m2 with
	| DimvarOneDimension(map1), DimvarOneDimension(map2) -> one_dimension_mapping_equal map1 map2

let dimvar_equal_commutative equivalence_map m1 m2 =
	match m1, m2 with
	| DimvarOneDimension(map1), DimvarOneDimension(map2) ->
			one_dimension_mapping_equal_commutative equivalence_map map1 map2

let dimvar_list_equal m1 m2 =
    let zipped = List.zip m1 m2 in
    match zipped with
    | Ok(l) -> List.for_all l (fun (x, y) -> dimvar_equal x y)
    | Unequal_lengths -> false

let dimvar_list_list_equal m1 m2 =
    let zipped = List.zip m1 m2 in
    match zipped with
    | Ok(l) -> List.for_all l (fun (x, y) -> dimvar_list_equal x y)
    | Unequal_lengths -> false

(* Assume that equality in the constraints => equality
    in the dimension.  *)
let dimension_constraint_equal m1 m2 =
    match m1, m2 with
    | DimensionConstraints(m1con, _), DimensionConstraints(m2con, _) -> dimvar_equal m1con m2con

(* See notes on the other dimension_constraint_**_equal *)
let dimension_constraint_list_list_equal m1 m2 =
    let get_constraints x =
        let get_constraints' y =
            match y with
            | DimensionConstraints(c, _) -> c
        in
        List.map x get_constraints'
    in
    let m1_constraints = List.map m1 get_constraints in
    let m2_constraints = List.map m2 get_constraints in
    dimvar_list_list_equal m1_constraints m2_constraints

let skeleton_type_to_string stype =
	match stype with
	| SInt(name) -> "SInt(" ^ (name_reference_to_string name) ^ ")"
	| SBool(name) -> "SBool(" ^ (name_reference_to_string name) ^ ")"
	| SFloat(name) -> "SFloat(" ^ (name_reference_to_string name) ^ ")"
	| SString(name) -> "SString(" ^ (name_reference_to_string name) ^ ")"

(* For use looking thes variables up in maps.  *)
let skeleton_type_to_id_string stype =
    let nr = match stype with
    | SInt(nr) -> nr
    | SBool(nr) -> nr
    | SFloat(nr) -> nr
    | SString(nr) -> nr
    in
    name_reference_to_id_string nr

let assignment_type_equal ass1 ass2 =
	match ass1, ass2 with
	| AssignVariable(v1), AssignVariable(v2) -> name_reference_equal (StructName(v1)) (StructName(v2))
	| AssignConstant(c1), AssignConstant(c2) -> synth_value_equal c1 c2
	| _, _ -> false

let assignment_type_to_string stype =
    match stype with
    | AssignVariable(v) -> name_reference_list_to_string v
    | AssignConstant(c) ->
            "Constant(" ^ synth_value_to_string c ^ ")"

let assignment_type_to_id_string stype =
    match stype with
    | AssignVariable(v) -> name_reference_to_id_string (StructName(v))
    | AssignConstant(c) ->
            synth_value_to_string c

let type_of_assignment tmap stype =
	match stype with
	| AssignVariable(v) -> type_of_name_reference_list tmap v
	| AssignConstant(c) -> synth_value_to_type c

let assignment_type_list_to_string stype_list =
    String.concat ~sep:", " (List.map stype_list assignment_type_to_string)

let rec skeleton_dimension_group_type_to_string stype =
	match stype with
	| SType(subtype) -> skeleton_type_to_string subtype
	| STypes(subtype) -> "STypes(" ^ (String.concat ~sep:", " (List.map subtype skeleton_dimension_group_type_to_string)) ^ ")"
	| SArray(name, subdim, lenvar) -> "SArray(" ^ (name_reference_to_string name) ^ ": "
                                ^ (skeleton_dimension_group_type_to_string subdim) ^
								": with lenvar " ^ (dimension_type_to_string lenvar) ^ ")"

let skeleton_dimension_group_type_list_to_string typs =
	String.concat ~sep:", " (List.map typs skeleton_dimension_group_type_to_string)

let probability_table_to_string p =
	String.concat ~sep:", " (List.map (Hashtbl.keys p) (fun k ->
		k ^ ": " ^ (Float.to_string (Hashtbl.find_exn p k))
	)
	)
let skeleton_dimension_probabilistic_group_type_to_string stype =
	match stype with
	| Probability(sgroup, p) ->
			skeleton_dimension_group_type_to_string sgroup ^ " (with probabilities " ^ (probability_table_to_string p) ^ ") "

let skeleton_dimension_probabilistic_group_type_list_to_string stypes =
	String.concat ~sep:"\n" (List.map stypes skeleton_dimension_probabilistic_group_type_to_string)

let flat_single_variable_binding_to_string (binding: flat_single_variable_binding) =
	   "\nWith the array index wrappers " ^ (String.concat ~sep:"," (List.map binding.tovar_index_nesting name_reference_to_string)) ^
	   "\nAnd (fromvars) [" ^ (String.concat ~sep:"], [" 
		   (List.map binding.fromvars_index_nesting (assignment_type_to_string))) ^ "]" ^
       "\nUnder dimensions [" ^ (String.concat ~sep:", "
            (List.map binding.dimensions dimension_constraint_to_string)) ^ "]" ^
       "\nWith conversion function " ^ (conversion_function_to_string binding.conversion_function) 


let flat_single_variable_binding_list_to_string skels =
    String.concat ~sep:"\n" (
        List.map skels flat_single_variable_binding_to_string
    )

let flat_single_variable_binding_list_list_to_string skels =
    String.concat ~sep:"\n--- NEW LIST ---\n" (
        List.map skels flat_single_variable_binding_list_to_string
    )

let flat_skeleton_type_binding_to_string skeleton =
	"SKELETON:\n" ^ String.concat ~sep:"\n\n>(new binding): \n" (
	List.map skeleton.flat_bindings flat_single_variable_binding_to_string)

let flat_skeleton_list_to_string bindings =
	"FLAT BINDINGS:\n"^ (String.concat ~sep:"\n>(new binding): \n" (
		List.map bindings (fun bindings_for_var ->
			flat_skeleton_type_binding_to_string bindings_for_var
	))) ^ "\n"

let single_variable_binding_group_to_string (binding: single_variable_binding_option_group) =
	   "\nWith the array index wrappers " ^ (String.concat ~sep:"," (List.map binding.tovar_index_nesting name_reference_to_string)) ^
	   "\nAnd (fromvars) [" ^ (String.concat ~sep:"], [" 
		   (List.map binding.fromvars_index_nesting (assignment_type_to_string))) ^ "]" ^
       "\nUnder dimensions [" ^ (String.concat ~sep:", "
            (List.map binding.dimensions_set (fun dimset ->
                String.concat ~sep:" or " (List.map dimset dimension_constraint_to_string)))) ^ "]" ^
	   "\nAnd Probability " ^ (Float.to_string binding.probability)

let single_variable_binding_list_to_string binds =
	"SKELETON:\n" ^ String.concat ~sep:"\n" (
	List.map binds single_variable_binding_group_to_string)

let skeleton_type_binding_to_string binds =
    single_variable_binding_list_to_string binds.bindings

let skeleton_dimension_group_types_to_string typs =
    String.concat ~sep:", DimensionType:" (List.map typs skeleton_dimension_group_type_to_string)

let skeleton_type_is_annon ty =
    let nr = 
    match ty with
            | SInt(nr) -> nr
            | SBool(nr) -> nr
            | SFloat(nr) -> nr
            | SString(nr) -> nr
    in
    name_reference_is_annon nr

let skeleton_dimension_group_type_is_annon ty =
    match ty with
    | SType(sty) -> skeleton_type_is_annon sty
    | STypes(stys) -> false (* I mean, they could syntactically be, but that would definitely be an error. *)
    | SArray(_, _, _) -> false

let skeleton_dimension_group_type_to_id_string ty =
    match ty with
    | SType(sty) -> skeleton_type_to_id_string sty
    | STypes(stys) -> assert false (* Shouldn't have this --- we can'g generate a single ID string from a list of types.  *)
    | SArray(nam, stys, dim) ->
            if skeleton_dimension_group_type_is_annon stys then
                (* if this is just int[] (e.g.), then we don't
                want the random AnnonymousName on the end.  *)
                (name_reference_to_string nam)
            else
                (name_reference_to_string nam) ^ "." ^ (skeleton_dimension_group_type_to_string stys)


let skeleton_list_to_string bindings =
	"BINDINGS:\n"^ (String.concat ~sep:">(new binding): \n" (
		List.map bindings (fun bindings_for_var ->
			skeleton_type_binding_to_string bindings_for_var
	))) ^ "\n"

let double_binding_options_list_to_string opts =
	skeleton_list_to_string (List.map opts (fun o -> { bindings = o }))

let skeletons_to_string skeletons =
	String.concat ~sep:"\n" (List.map skeletons skeleton_type_binding_to_string)

let flat_skeleton_pairs_to_string skeletons =
	String.concat ~sep:"\n" (List.map skeletons (fun (pre, post) ->
		"Pre: " ^ (flat_skeleton_type_binding_to_string pre) ^
		"\n\nPost" ^ (flat_skeleton_type_binding_to_string post)))

let flat_skeleton_pairs_and_ranges_to_string skeletons =
	String.concat ~sep:"\n" (List.map skeletons (fun (skeleton) ->
		"Pre: " ^ (flat_skeleton_type_binding_to_string skeleton.pre) ^
		"\nPost: " ^ (flat_skeleton_type_binding_to_string skeleton.post) ^
		(match skeleton.rangecheck with
		| None -> ""
		| Some(range) ->
				"\nRangeCheck: " ^ (conditional_to_string range)
		)
	))

let skeleton_pairs_to_string (pairs: skeleton_pairs) =
	"Pre: " ^ (flat_skeleton_type_binding_to_string pairs.pre) ^
	"\nPost: " ^ (flat_skeleton_type_binding_to_string pairs.post)

let typesets_to_string t =
	String.concat ~sep:") List (" (List.map t (fun t -> (String.concat ~sep:", " (List.map t skeleton_type_to_string))))

let types_to_string t =
	String.concat ~sep:", " (List.map t skeleton_type_to_string)

let index_nesting_to_string nest =
    name_reference_list_to_string nest

let assignment_type_equal a1 a2 =
    match a1, a2 with
    | AssignConstant(c1), AssignConstant(c2) ->
            synth_value_equal c1 c2
    | AssignVariable(v1), AssignVariable(v2) ->
            name_reference_list_equal v1 v2
    | _, _ -> false

(* Apparently we don't care about list ordering? *)
(* I mean, allowing for unequal ordering would 100% make
this much better --- just a bit of a question of if it's
worth the effot/runtime.  *)
let assignment_type_list_equal l1 l2 =
    let res = List.zip l1 l2 in
    match res with
    | Ok(rlist) -> List.for_all rlist (fun (a1, a2) -> assignment_type_equal a1 a2)
    | Unequal_lengths -> false

let single_variable_binding_equal (s1: single_variable_binding_option_group) (s2: single_variable_binding_option_group) =
    (assignment_type_list_equal s1.fromvars_index_nesting s2.fromvars_index_nesting) &&
    (name_reference_list_equal s1.tovar_index_nesting s2.tovar_index_nesting) &&
    (dimension_constraint_list_list_equal s1.dimensions_set s2.dimensions_set)

let name_refs_from_skeleton sk =
	match sk with
	| SInt(nr) -> nr
	| SBool(nr) -> nr
	| SFloat(nr) -> nr
	| SString(nr) -> nr


let var_match_equal (nr1, nr2, dim_rel) (nr1', nr2', dim_rel') =
	(name_reference_equal nr1 nr1') &&
	(name_reference_equal nr2 nr2') &&
	(dim_relation_equal dim_rel dim_rel')
