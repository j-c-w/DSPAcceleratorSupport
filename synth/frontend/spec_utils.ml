open Core_kernel;;
open Spec_definition;;
open Range_definition;;

exception SpecException of string

let rec name_reference_to_string nref =
	match nref with
	| AnonymousName -> "Annon"
	| Name(s) -> s
	| StructName(ns) -> (String.concat ~sep:"." (List.map ns name_reference_to_string))

let name_reference_list_to_string nrefs =
	String.concat ~sep:", " (List.map nrefs name_reference_to_string)

let name_reference_option_list_to_string nrefs =
	String.concat ~sep:", " (List.map nrefs (fun n ->
		match n with
		| None -> "None"
		| Some(n) -> name_reference_to_string n))

let rec name_reference_equal n1 n2 =
    match (n1, n2) with
	| AnonymousName, AnonymousName -> true
    | Name(x), Name(y) -> (String.compare x y) = 0
    | StructName(xns), StructName(yns) -> (
            match List.zip xns yns with
            | Ok(nms) -> List.for_all nms (fun (a, b) -> name_reference_equal a b)
            | Unequal_lengths -> false
	)
	(* Assume no funny-business with
	oddly nested structnames *)
	| _, _ -> false

let rec dimension_type_to_string dim =
    match dim with
    | EmptyDimension -> "No dimensions set!"
    | Dimension(nrefs) ->
            (String.concat ~sep:", " (List.map nrefs name_reference_to_string))

let empty_dimension dim = match dim with
	| EmptyDimension -> true
	| _ -> false

let dimension_type_list_to_string dims =
	String.concat ~sep:"DIM: " (
		List.map dims dimension_type_to_string
	)

let dimension_type_equal d1 d2 = match d1, d2 with
    | EmptyDimension, EmptyDimension -> true
    | Dimension(nlist1), Dimension(nlist2) -> (
            match List.zip nlist1 nlist2 with
            | Ok(l) ->
                    List.for_all l (fun (a, b) -> name_reference_equal a b)
            | Unequal_lengths -> false
	)
	| _ -> false

let rec synth_type_to_string t =
    match t with
    | Int16 -> "int16"
    | Int32 -> "int32"
    | Int64 -> "int64"
    | Float16 -> "float16"
    | Float32 -> "float32"
    | Float64 -> "float64"
    | Array(x, dims) -> "array(" ^ (synth_type_to_string x) ^ ": with dims " ^
        (dimension_type_to_string dims) ^ ")"
    | Struct(name) -> name
    | Fun(from, fto) -> (synth_type_to_string from) ^ "->" ^ (synth_type_to_string fto)
	| Unit -> "unit"

let rec synth_type_equal s1 s2 =
	match s1, s2 with
	| Int16, Int16 -> true
	| Int32, Int32 -> true
	| Int64, Int64 -> true
	| Float16, Float16 -> true
	| Float32, Float32 -> true
	| Float64, Float64 -> true
	| Array(x, dms), Array(x2, dms2) ->
			(synth_type_equal x x2) &&
			(dimension_type_equal dms dms2)
	| Struct(nm), Struct(nm2) ->
			(String.compare nm nm2) = 0
	| Unit, Unit -> true
	| Fun(f, t), Fun(f2, t2) ->
			(synth_type_equal f f2) &&
			(synth_type_equal t t2)
	| _ -> false

let rec synth_value_to_string value =
    match value with
    | Int16V(v) -> string_of_int v
    | Int32V(v) -> string_of_int v
    | Int64V(v) -> string_of_int v
    | Float16V(v) -> string_of_float v
    | Float32V(v) -> string_of_float v
    | Float64V(v) -> string_of_float v
    | UnitV -> "Unit"
    | ArrayV(vs) ->
            "[" ^ (String.concat ~sep:", " (List.map vs synth_value_to_string)) ^ "]"
    | StructV(name, _) ->
            name
    | FunV(name) ->
            name

let synth_value_from_range_value rvalue =
    match rvalue with
    | RInt(v) -> Int32V(v)
    | RFloat(v) -> Float32V(v)

let type_hash_table_to_string (type_hash: (string, synth_type) Hashtbl.t) =
	let keys = Hashtbl.keys type_hash in
    String.concat ~sep:", " (
	List.map
	(List.map keys (fun key -> (key, Hashtbl.find_exn type_hash key)))
		(fun (k, typ) -> k ^ ": " ^ (synth_type_to_string typ)))

let iospec_to_string (iospec: iospec) =
    "Livein: " ^ (String.concat ~sep:", " iospec.livein) ^
    "\nLiveout: " ^ (String.concat ~sep:", " iospec.liveout) ^
    "\nexeccmd: " ^ iospec.execcmd ^
	"\ntypemap: " ^ (type_hash_table_to_string iospec.typemap) ^
	"\n"

let apispec_to_string (apispec: apispec) =
    "Livein: " ^ (String.concat ~sep:", " apispec.livein) ^
    "\nLiveout: " ^ (String.concat ~sep:", " apispec.liveout) ^
    "\nexeccmd: " ^ apispec.execcmd ^
	"\ntypemap: " ^ (type_hash_table_to_string apispec.typemap) ^
	"\n"

let get_class_typemap smeta =
	match smeta with
	| ClassMetadata(cls) -> cls.typemap
	| StructMetadata(str) -> str.typemap
let get_class_members smeta =
	match smeta with
	| ClassMetadata(cls) -> cls.members @ cls.functions
	| StructMetadata(str) -> str.members
let get_class_fields smeta =
	match smeta with
	| ClassMetadata(cls) -> cls.members
	| StructMetadata(str) -> str.members
let is_class smeta =
	match smeta with
	| ClassMetadata(_) -> true
	| StructMetadata(_) -> false

let name_reference_list_equal n1 n2 =
	let zipped = List.zip n1 n2 in
	match zipped with
	| Ok(l) -> List.for_all l (fun (x, y) -> name_reference_equal x y)
	| Unequal_lengths -> false

let name_reference_list_list_equal n1 n2 = 
	let zipped = List.zip n1 n2 in
	match zipped with
	| Ok(l) -> List.for_all l (fun (x, y) -> name_reference_list_equal x y)
	| Unequal_lengths -> false

let name_reference_is_struct nr =
	match nr with
	| AnonymousName -> false
	| Name(_) -> false
	| StructName(_) -> true

let name_reference_base_name nr =
	match nr with
	| AnonymousName -> raise (SpecException "Can't get base name of anon")
	| Name(_) -> nr
	| StructName(x :: xs) -> x
	| StructName([]) -> raise (SpecException "can't have empty strucname")

let is_float_type typ =
	match typ with
	| Float16 -> true
	| Float32 -> true
	| Float64 -> true
	| _ -> false

let is_integer_type typ =
	match typ with
	| Int16 -> true
	| Int32 -> true
	| Int64 -> true
	| _ -> false

(* Given a typename of the format x.y.z, determine the type
   of z.  Note taht this is a type specification string,
   not a reference string, so above, it's perfectly
   fine for y to be an array.  *)
let type_of typmap classmap k =
    let rec typeloop currtypmap tps = match tps with
    (* Think this isn't possible anyway.  *)
    | [] -> raise (SpecException "Cant have empty type")
    | x :: y :: ys ->
            (* Get the next typmap *)
            let current_typ = Hashtbl.find_exn currtypmap x in
            let class_name = (synth_type_to_string current_typ) in
            let this_classmap = get_class_typemap (Hashtbl.find_exn classmap class_name) in
            typeloop this_classmap (y :: ys)
    | x :: xs -> 
            (* Just the type of x. *)
            Hashtbl.find_exn currtypmap x
    in
    typeloop typmap (String.split_on_chars k ['.'])
