open Skeleton_definition;;
open Spec_definition;;
open Spec_utils;;
open Core_kernel;;

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

let flat_single_variable_binding_to_string (binding: flat_single_variable_binding) =
	   "\nWith the array index wrappers " ^ (String.concat ~sep:"," (List.map binding.tovar_index_nesting name_reference_to_string)) ^
	   "\nAnd (fromvars) [" ^ (String.concat ~sep:"], [" 
		   (List.map binding.fromvars_index_nesting (fun x -> (String.concat ~sep:" ," (List.map x name_reference_to_string))))) ^ "]"

let flat_single_variable_binding_list_to_string skels =
    String.concat ~sep:"\n" (
        List.map skels flat_single_variable_binding_to_string
    )

let flat_single_variable_binding_list_list_to_string skels =
    String.concat ~sep:"\n--- NEW LIST ---\n" (
        List.map skels flat_single_variable_binding_list_to_string
    )

let flat_skeleton_type_binding_to_string skeleton =
	"SKELETON:\n" ^ String.concat ~sep:"\n" (
	List.map skeleton.flat_bindings flat_single_variable_binding_to_string)

let flat_skeleton_list_to_string bindings =
	"FLAT BINDINGS:\n"^ (String.concat ~sep:"\n>(new binding): \n" (
		List.map bindings (fun bindings_for_var ->
			flat_skeleton_type_binding_to_string bindings_for_var
	))) ^ "\n"

let single_variable_binding_to_string (binding: single_variable_binding_option_group) =
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

let flat_skeleton_pairs_to_string skeletons =
	String.concat ~sep:"\n" (List.map skeletons (fun (pre, post) ->
		"Pre: " ^ (flat_skeleton_type_binding_to_string pre) ^
		"\n\nPost" ^ (flat_skeleton_type_binding_to_string post)))

let typesets_to_string t =
	String.concat ~sep:") List (" (List.map t (fun t -> (String.concat ~sep:", " (List.map t skeleton_type_to_string))))

let types_to_string t =
	String.concat ~sep:", " (List.map t skeleton_type_to_string)

let single_variable_binding_equal s1 s2 =
	s1 = s2