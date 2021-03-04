open Skeleton_definition;;
open Spec_definition;;
open Spec_utils;;
open Core_kernel;;
open Builtin_conversion_functions;;

let one_dim_var_mapping_to_string map =
	match map with
	| ExactVarMatch(fromv, tov) -> (name_reference_to_string fromv) ^
    " = " ^ (name_reference_to_string tov)

let rec dimvar_mapping_to_string mapping = match mapping with
	| DimvarOneDimension(map) -> one_dim_var_mapping_to_string map

let dimvar_mapping_list_to_string mapping =
    String.concat ~sep:"\n" (List.map mapping dimvar_mapping_to_string)

let one_dimension_mapping_equal m1 m2 =
    match m1, m2 with
    | ExactVarMatch(fromv1, tov1), ExactVarMatch(fromv2, tov2) ->
            (name_reference_equal fromv1 fromv2) &&
            (name_reference_equal tov1 tov2)

let dimvar_equal m1 m2 =
	match m1, m2 with
	| DimvarOneDimension(map1), DimvarOneDimension(map2) -> one_dimension_mapping_equal map1 map2

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

let flat_single_variable_binding_to_string (binding: flat_single_variable_binding) =
	   "\nWith the array index wrappers " ^ (String.concat ~sep:"," (List.map binding.tovar_index_nesting name_reference_to_string)) ^
	   "\nAnd (fromvars) [" ^ (String.concat ~sep:"], [" 
		   (List.map binding.fromvars_index_nesting (fun x -> (String.concat ~sep:" ," (List.map x name_reference_to_string))))) ^ "]" ^
       "\nUnder dimensions [" ^ (String.concat ~sep:", "
            (List.map binding.valid_dimensions dimvar_mapping_to_string)) ^ "]" ^
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

let single_variable_binding_to_string (binding: single_variable_binding_option_group) =
	   "\nWith the array index wrappers " ^ (String.concat ~sep:"," (List.map binding.tovar_index_nesting name_reference_to_string)) ^
	   "\nAnd (fromvars) [" ^ (String.concat ~sep:"], [" 
		   (List.map binding.fromvars_index_nesting (fun x -> (String.concat ~sep:" ," (List.map x name_reference_to_string))))) ^ "]" ^
       "\nUnder dimensions [" ^ (String.concat ~sep:", "
            (List.map binding.valid_dimensions_set (fun dimset ->
                String.concat ~sep:" or " (List.map dimset dimvar_mapping_to_string)))) ^ "]"

let single_variable_binding_list_to_string binds =
	"SKELETON:\n" ^ String.concat ~sep:"\n" (
	List.map binds single_variable_binding_to_string)

let skeleton_type_binding_to_string binds =
    single_variable_binding_list_to_string binds.bindings

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

let flat_skeleton_pairs_to_string skeletons =
	String.concat ~sep:"\n" (List.map skeletons (fun (pre, post) ->
		"Pre: " ^ (flat_skeleton_type_binding_to_string pre) ^
		"\n\nPost" ^ (flat_skeleton_type_binding_to_string post)))

let typesets_to_string t =
	String.concat ~sep:") List (" (List.map t (fun t -> (String.concat ~sep:", " (List.map t skeleton_type_to_string))))

let types_to_string t =
	String.concat ~sep:", " (List.map t skeleton_type_to_string)

let index_nesting_to_string nest =
    name_reference_list_to_string nest

let single_variable_binding_equal (s1: single_variable_binding_option_group) (s2: single_variable_binding_option_group) =
    (name_reference_list_list_equal s1.fromvars_index_nesting s2.fromvars_index_nesting) &&
    (name_reference_list_equal s1.tovar_index_nesting s2.tovar_index_nesting) &&
    (dimvar_list_list_equal s1.valid_dimensions_set s2.valid_dimensions_set)
