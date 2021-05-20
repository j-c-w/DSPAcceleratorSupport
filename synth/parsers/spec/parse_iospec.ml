(* This file parses information about APIs *)
open Core_kernel;;
open Yojson;;
open Yojson.Basic.Util;;
open Spec_definition;;
open Spec_utils;;
open Parse_classmap;;
open Parse_type;;
open Parse_range;;
open Parse_const;;
open Options;;

exception IOSpecError of string

let extract_typemap typemap vars =
	let tbl: (string, synth_type) Hashtbl.t = (Hashtbl.create (module String)) in
	(* Exctract the type names and their values from the JSON. *)
	let typepairs = List.map vars (fun var -> (var, parse_type(typemap |> member var |> to_string))) in
	(* Add these types to the hash map.  *)
	ignore(List.map typepairs (fun (var, t)  -> Hashtbl.add tbl var t));
	tbl;;

let load_iospec options filename =
	let json = Yojson.Basic.from_file filename in
	let classmap = load_classmap_from_json (json |> member "classmap") in
	let livein = List.map (json |> member "livein" |> to_list) (fun j -> j |> to_string) in
	let liveout = List.map (json |> member "liveout" |> to_list) (fun j -> j |> to_string) in
	let retvars = json |> member "returnvarname" |> to_string_option in
	let execcmd = json |> member "execcmd" |> to_string in
	let typemap = extract_typemap (json |> member "typemap") (livein @ liveout) in
    let funname = json |> member "funname" |> to_string in
	let funargs = List.map (json |> member "funargs" |> to_list) (fun j -> j |> to_string) in
	let compiler_flags = match json |> member "compiler_flags" with
    | `Null -> []
    | other -> List.map (other |> to_list) to_string in
	let required_includes = List.map (json |> member "required_includes" |> to_list) to_string in
	let range_tbl = load_rangetable options classmap typemap (json |> member "range") in
	let valid_tbl = load_rangetable options classmap typemap (json |> member "valid") in
	let const_tbl = load_consttable options (json |> member "consts") in
	let iospec: iospec = {
		livein=livein;
		liveout=liveout;
		execcmd=execcmd;
		funname=funname;
		funargs=funargs;
		compiler_flags=compiler_flags;
		required_includes=required_includes;
		rangemap = range_tbl;
		validmap = valid_tbl;
		constmap = const_tbl;
		returnvar=retvars;
	} in
	let () =
		if options.debug_load then
			Printf.printf "Loaded iospec is %s" (iospec_to_string iospec)
		else () in
	iospec, typemap, classmap
