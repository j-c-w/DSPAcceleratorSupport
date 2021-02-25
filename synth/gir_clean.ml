open Core_kernel;;
open Gir;;
open Gir_utils;;


let rec gir_double_define_clean_internal deftbl gir =
	match gir with
	| Definition(n) ->
			(* See if already defined *)
			let defined =
				Hashtbl.find deftbl (gir_name_to_string n) in
			(* Set defined.  *)
			let () =
				Hashtbl.set deftbl (gir_name_to_string n) true in
			(
			match defined with
				| Some(otherdef) -> EmptyGIR
				| None -> gir
			)
	| Sequence(girs) ->
			Sequence(List.map girs (gir_double_define_clean_internal deftbl))
	| Assignment(_, _) ->
			gir
	| LoopOver(body, indv, maxv) ->
			let defined = Hashtbl.find deftbl (gir_name_to_string indv) in
			let () = Hashtbl.set deftbl (gir_name_to_string indv) true in
			(* Can't have the ind vars defined more than once.
			Could replace this with a clever-er fix, e.g.
			replacing the vars.  *)
			let () = assert (defined = None) in
			LoopOver(gir_double_define_clean_internal deftbl body, indv, maxv)
	| Expression(_) ->
			gir
	| EmptyGIR -> EmptyGIR

(* Given some GIR, remove things that are defined more than once.
This is produced as an artifact of the generate_gir phase.
*)
let gir_double_define_clean gir =
	let deftbl = Hashtbl.create (module String) in
	gir_double_define_clean_internal deftbl gir
