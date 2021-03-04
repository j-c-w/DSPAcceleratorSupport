open Core_kernel;;
open Options;;
open Gir;;

(* GIR reduce is a simple GIR pass that removes trivial artefacts
from other passes, e.g. nested sequences, deletes EmptyGIR etc. *)

(* Note that we can assume that the C compiler will do a lot
   of optimization so we don't have to do those here for the
   sake of the C compiler --- we need to simplify things so
   that future GIR passes can have less work to do.  *)
let rec reduce_rvalue (options: options) (rval: rvalue): rvalue =
    match rval with
    | Expression(expr) ->
            Expression(reduce_expression options expr)
and reduce_expression (options: options) expr: expression =
    match expr with
    | VariableReference(_) -> expr
	(* Remove calls to the identity function.  *)
    | FunctionCall(FunctionRef(Name("identity")), VariableList([v])) ->
                    VariableReference(v)
    | FunctionCall(_, _) -> expr
    | GIRMap(_, _) -> expr

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
			if (List.is_empty filtered) then
				EmptyGIR
			else
				Sequence(filtered)
	| LoopOver(gir, ind, max) ->
			LoopOver((reduce_gir options gir), ind, max)
	| FunctionDef(name, args, body, typmap) ->
			FunctionDef(name, args, (reduce_gir options body), typmap)
    | Expression(expr) ->
            Expression(reduce_expression options expr)
    | Assignment(ton, fromn) ->
            Assignment(ton, reduce_rvalue options fromn)
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
