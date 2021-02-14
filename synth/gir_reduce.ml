open Core_kernel;;
open Options;;
open Gir;;

(* GIR reduce is a simple GIR pass that removes trivial artefacts
from other passes, e.g. nested sequences, deletes EmptyGIR etc. *)

(* Note that we can assume that the C compiler will do a lot
   of optimization so we don't have to do those here for the
   sake of the C compiler --- we need to simplify things so
   that future GIR passes can have less work to do.  *)
let rec reduce_gir (options: options) gir: gir =
	match gir with
	| Sequence(subitems) ->
			let flattened = List.concat (List.map subitems (fun subitem ->
				match subitem with
				| Sequence(subseqs) ->
						let sub_exed = List.map subseqs (reduce_gir options) in
						(* Now, remove the subseqs. *)
						let unsequed = List.concat (
							List.map sub_exed (fun f ->
								match f with
								| Sequence(xs) -> xs
								| x -> [x]
							)
						) in
						unsequed
				| x -> [x]
			)
			)
			in
			let filtered = List.filter flattened (fun sub ->
				match sub with
				| EmptyGIR -> false
				| _ -> true
			)
			in
			if filtered = [] then
				EmptyGIR
			else
				Sequence(filtered)
	| LoopOver(gir, ind, max) ->
			LoopOver((reduce_gir options gir), ind, max)
	| x -> x

let rec reduce_gir_check gir =
	match gir with
	| Sequence(subseq) ->
			let _ = List.map subseq (
				fun x ->
					match x with
					| Sequence(_) -> assert (false)
					| _ -> ()
			) in ()
	| _ -> ()
