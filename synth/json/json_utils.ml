open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Yojson;;
open Yojson.Basic.Util;;

exception JSONException of string

let rec load_json j =
	let mempairs = List.map j (fun (name, elt) ->
		(name, load_json_elt (elt))
	) in
    let result = Hashtbl.create (module String) in
    let _ = List.map mempairs (fun (name, value) ->
        Hashtbl.add result name value
    ) in
    result

and load_json_elt (e: Yojson.Basic.t) =
	match e with
	| `Assoc(d) ->
			StructV("Unknown", load_json d)
	| `Bool(b) ->
			(* Not generated right now? *)
			raise (JSONException "Unepxected type: bool")
	| `Float(f1) ->
			(* arbitrarily choose this I suppose --- note that
			 if this changes, there are a bunch of other synth
			 things that have to change. *)
			Float64V(f1)
	| `Int(i) ->
			Int32V(i)
	| `List(l) ->
			ArrayV(List.map l load_json_elt)
	| `Null ->
			raise (JSONException "Loading Null JSON")
	| `String(s) ->
			raise (JSONException "Unexpected type: string")

let load_value_map_from file =
	let json = Yojson.Basic.from_file file in
	let j_members = keys json in
    let json_elts = List.map j_members (fun mem ->
        (mem, json |> member mem)
    ) in
	load_json json_elts
