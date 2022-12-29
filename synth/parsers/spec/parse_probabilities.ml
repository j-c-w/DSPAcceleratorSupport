open Core;;
open Yojson;;
open Yojson.Basic.Util;;
open Spec_definition;;
open Parse_type;;
open Parse_range;;
open Parse_classmap;;

exception ProbabilityException of string

let load_binding_specification specname =
	match specname with
	| None -> None
	| Some(name) -> Some(
		let json = Yojson.Basic.from_file name in
		(* Walk through the json *)
		let base_map = Hashtbl.create (module String) in
		let iospec_vars = json |> keys in
		let _ = List.map iospec_vars ~f:(fun v ->
			(* Sanity-check it's in the IOSpec *)
			let prob_map = Hashtbl.create (module String) in
			let sub_dict = match json |> member v with
			| `Assoc(s) -> s
			| _ -> raise (ProbabilityException "Unallowed non-dict")
			in
			let _ = List.map (sub_dict) ~f:(fun (name, prob) ->
				(* Get the probability and setup the map.  *)
				let _ = match prob with
				| `Float(v) -> 
						Hashtbl.add prob_map ~key:name ~data:v
				| other -> raise (ProbabilityException ("Error: Non-FP type encountered" ^ (Yojson.Basic.show other)))
				in
				()
			) in
			Hashtbl.add base_map ~key:v ~data:prob_map
		) in
		{ probabilities = base_map; }
	)
