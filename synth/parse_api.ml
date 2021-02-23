open Core_kernel;;
open Yojson;;
open Yojson.Basic.Util;;
open Spec_definition;;
open Parse_type;;
open Parse_typemap;;

let load_target_api filename: apispec =
	let json = Yojson.Basic.from_file filename in
	let livein = List.map (json |> member "livein" |> to_list) to_string in
	let execcmd = json |> member "execcmd" |> to_string in
	let liveout = List.map (json |> member "liveout" |> to_list) to_string in
	let typemap = load_typemap json (livein @ liveout) in
	let funname = json |> member "functionname" |> to_string in
	let funargs = List.map (json |> member "functionargs" |> to_list) to_string in
	let required_includes = List.map (json |> member "required_includes" |> to_list) to_string in
	{
		livein = livein;
		liveout=liveout;
		execcmd=execcmd;
		typemap=typemap;
		funname = funname;
		funargs = funargs;
		required_includes = required_includes;
	};;
