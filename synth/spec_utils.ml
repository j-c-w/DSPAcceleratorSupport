open Core_kernel;;
open Spec_definition;;

exception SpecException of string

let rec name_reference_to_string nref =
	match nref with
	| AnonymousName -> "Annon"
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

let rec name_reference_equal n1 n2 =
    match (n1, n2) with
	| AnonymousName, AnonymousName -> raise (SpecException "Can't compare annonymous and annonmous")
    | Name(x), Name(y) -> x = y
    | StructName(xns), StructName(yns) -> (
            match List.zip xns yns with
            | Ok(nms) -> List.for_all nms (fun (a, b) -> name_reference_equal a b)
            | Unequal_lengths -> false
	)
	(* Assume no funny-business with
	oddly nested structnames *)
	| _, _ -> false

let name_reference_is_struct nr =
	match nr with
	| AnonymousName -> false
	| Name(_) -> false
	| StructName(_) -> true

(*  checks if x is a chile of some class y, e.g.
 if y is X and x is X.a, then it's true*)
let rec name_reference_member x y =
	match (x, y) with
	| AnonymousName, _ -> raise (SpecException "Can't compare annonymous to anything")
	| _, AnonymousName -> raise (SpecException "Can't compare annonymous to anything")
	| Name(x), Name(y) -> x = y
	| StructName(x :: xs), Name(_) ->
			name_reference_member x y
	(* Really this shouldn't be possible,
	but it's a hack to keep the recursio nworking *)
	| StructName([]), StructName(_) -> true
	| StructName(x :: xs), StructName(y :: ys) ->
			(name_reference_equal x y) &&
			(name_reference_member (StructName(xs)) (StructName(ys)))
