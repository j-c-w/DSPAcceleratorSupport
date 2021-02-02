open Core_kernel;;

exception ParseException of string

type synth_type =
	| Int16 | Int32 | Int64
	| Float16 | Float32 | Float64
	| Array of synth_type
	| Unit
	| Struct of string
	| Fun of synth_type * synth_type

type synth_value =
    | Int16V of int
    | Int32V of int
    | Int64V of int
    | Float16V of float
    | Float32V of float
    | Float64V of float
    | UnitV
    | ArrayV of synth_value list
    | StructV of string * (string, synth_value) Hashtbl.t
    | FunV of string

type iospec = {
	livein: string list;
	liveout: string list;
	execcmd: string;
	typemap: (string, synth_type) Hashtbl.t
}

type apispec = {
    livein: string list;
    liveout: string list;
    execcmd: string;
    typemap: (string, synth_type) Hashtbl.t
}

type classtype = {
	members: string list;
	functions: string list;
	typemap: (string, synth_type) Hashtbl.t
}

type structtype = {
	members: string list;
	typemap: (string, synth_type) Hashtbl.t
}

type structure_metadata =
	| ClassMetadata of classtype
	| StructMetadata of structtype
