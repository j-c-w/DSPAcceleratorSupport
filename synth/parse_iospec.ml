(* This file parses information about APIs *)
open Core_kernel;;
open Yojson;;
open Yojson.Basic.Util;;
open Spec_definition;;
open Parse_type;;

let extract_typemap typemap vars =
	let tbl: (string, synth_type) Hashtbl.t = (Hashtbl.create (module String)) in
	(* Exctract the type names and their values from the JSON. *)
	let typepairs = List.map vars (fun var -> (var, parse_type(typemap |> member var |> to_string))) in
	(* Add these types to the hash map.  *)
	ignore(List.map typepairs (fun (var, t)  -> Hashtbl.add tbl var t));
	tbl;;

let load_iospec filename: iospec =
	let json = Yojson.Basic.from_file filename in
	let livein = List.map (json |> member "livein" |> to_list) (fun j -> j |> to_string) in
	let liveout = List.map (json |> member "liveout" |> to_list) (fun j -> j |> to_string) in
	let execcmd = json |> member "execcmd" |> to_string in
	let typemap = extract_typemap (json |> member "typemap") (livein @ liveout) in
	{livein=livein; liveout=liveout; execcmd=execcmd; typemap=typemap}
