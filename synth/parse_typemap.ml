open Core_kernel;;
open Yojson;;
open Yojson.Basic.Util;;
open Spec_definition;;
open Parse_type;;

let load_typemap json_definition typenames =
	let typemap = Hashtbl.create (module String) in
	(* Get the types parsed *)
	let typemap_pairs = List.map typenames (fun name -> (name, json_definition |> member "typemap" |> member name |> to_string |> parse_type)) in
	(* Put the parsed types in a hash table. *)
	ignore(List.map typemap_pairs (fun (name, typ) -> Hashtbl.add typemap name typ));
	typemap
