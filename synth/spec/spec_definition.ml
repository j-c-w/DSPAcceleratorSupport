open Core_kernel;;
open Range_definition;;

exception ParseException of string

(* We need to have recursive types to store the name mappings,
   since that's what the type inputs that we take can be.  *)
type name_reference =
	| AnonymousName
	| Name of string
	(* To represent class names and member variables.  This
	   DOES NOT mean the list of all members of a class, but
	   rather the list of member names you have to traverse
	   to get to the member. *)
	| StructName of name_reference list

(* This is the way that the type stream is passed in.  *)
type synth_type =
	| Bool
	| Int16 | Int32 | Int64
	| Float16 | Float32 | Float64
	| Array of synth_type * dimension_type
	| Unit
	| Struct of string
	| Fun of synth_type * synth_type
and dimension_type =
	(* For each array variable, this keeps the
	   /prospective/ dimensions that it could have. *)
	(* This should be assigned in the assign_dimensions pass##
	or can be specified by the .  *)
    | EmptyDimension
	| Dimension of dimension_value list
and dimension_value =
	| DimVariable of name_reference
	| DimConstant of int

type synth_value =
	| BoolV of bool
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
	funname: string;
	livein: string list;
	liveout: string list;
	(* This should be an element of liveout, or empty.  *)
	(* This of course does not reflex 'real' C --- we don't
	try to do that here --- the compiler framework can
	easily change this to a traditional return variable
	and it means we don't have to deal with return types
	in the JSON formats.  We do perhaps need a pointer
	type to implement this correctly, but that is a future
	challenge.  *)
    returnvar: string option;
    funargs: string list;
	execcmd: string;
	required_includes: string list;
	typemap: (string, synth_type) Hashtbl.t;
	rangemap: (string, range_set) Hashtbl.t;
	validmap: (string, range_set) Hashtbl.t
}

type apispec = {
    livein: string list;
    liveout: string list;
    execcmd: string;
	funname: string;
	funargs: string list;
	required_includes: string list;
    compiler_flags: string list;
	typemap: (string, synth_type) Hashtbl.t;
	validmap: (string, range_set) Hashtbl.t
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