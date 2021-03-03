(* This file contains the parser for the type information. *)
open Core_kernel;;
open Yojson;;
open Yojson.Basic.Util;;
open Spec_definition;;
open Parse_type;;
open Synthesize;;
open Parse_typemap;;

let load_individual_type json_definition =
	let isstruct = json_definition |> member "type" |> to_string in
	let symbols = List.map (json_definition |> member "symbols" |> to_list) (fun j -> j |> to_string) in
	let functions =
		if (String.compare isstruct "class") = 0 then
			List.map (json_definition |> member "functions" |> to_list) to_string
			(* No functions in a struct.  *)
		else []
	in
	let typemap_members = symbols @ functions in
	let typemap = load_typemap json_definition typemap_members in
	if (String.compare isstruct "class") = 0 then
        ClassMetadata({members=symbols; functions=functions; typemap=typemap})
	else
        StructMetadata({members=symbols; typemap=typemap})

let load_classmap filename =
	let json = Yojson.Basic.from_file filename in
	let tbl = Hashtbl.create (module String) in
	(* Get the names of all the defined types.  *)
	let typedefs = json |> keys in
	(* Get the types from these definitions.  *)
	let typepairs = List.map typedefs (fun name -> (name, load_individual_type(json |> member name))) in
	ignore(List.map typepairs (fun (name, value) -> Hashtbl.add tbl name value));
	tbl;;
