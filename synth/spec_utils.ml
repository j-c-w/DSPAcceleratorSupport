open Core_kernel;;
open Spec_definition;;

let rec name_reference_to_string nref =
	match nref with
	| Name(s) -> s
	| StructName(ns) -> (String.concat ~sep:"." (List.map ns name_reference_to_string))

let name_reference_list_to_string nrefs =
	String.concat ~sep:", " (List.map nrefs name_reference_to_string)

let rec dimension_type_to_string dim =
    match dim with
    | EmptyDimension -> "No dimensions set!"
    | Dimension(nrefs) ->
            (String.concat ~sep:", " (List.map nrefs name_reference_to_string))
    | HigherDimention(subdim, dimvars) ->
            "[" ^ (dimension_type_to_string (Dimension(dimvars))) ^ "]" ^
            (dimension_type_to_string subdim)

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
