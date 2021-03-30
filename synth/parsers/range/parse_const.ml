open Core_kernel;;
open Parse_range;;
open Parse_type;;
open Yojson.Basic.Util;;
open Yojson;;
open Range;;

exception ConstTableException of string

let load_consttable json =
    let result = Hashtbl.create (module String) in
	let () = match json with
	| `Assoc(_) ->
		let keys = json |> keys in
		let _ = ignore(
			List.map keys (fun key ->
			(* Make sure that each key is a type --- after that
			   it doesn't matter to much because they are just
			   indexes into the hash table.  *)
				let typ = parse_type key in
				let range = List.map (json |> member key |> to_list) (to_string_option) in
				let parsed_range = List.filter_map range (fun x -> Option.map x (parse_range typ)) in
				let range_value_list = List.concat (
					List.map parsed_range range_values
				) in
				let synth_values =
					List.map range_value_list range_value_to_synth_value in
				Hashtbl.set result key synth_values
			)
		) in
		()
	(* Const table not specified.  *)
	| `Null -> ()
	| _ -> raise (ConstTableException "Unexpected JSON format for constable")
	in
	result
