open Core;;
open Gir;;
open Gir_utils;;
open Spec_definition;;
open Spec_utils;;
open Options;;

exception EvaluationException of string

(* This is completely incomplete --- to be complete,
it will need to be able to do things like call eternally
defined functions, which will be a bit of a challenge. *)
(* Anyway, the partially complete version has uses. *)

let eval_binary_comp options comp (v1: synth_value) (v2: synth_value) =
    let () = if options.debug_evaluate_gir then
		Printf.printf "Comparing %s and %s\n" (synth_value_to_string v1) (synth_value_to_string v2)
    else () in
	let comp_result =
		if (is_float_value v1) && (is_float_value v2) then
			let v1_f = Option.value_exn (float_from_value v1) in
			let v2_f = Option.value_exn (float_from_value v2) in
			Float.compare v1_f v2_f
		else if (is_int_value v1) && (is_int_value v2) then
			let v1_i = Option.value_exn (int_from_value v1) in
			let v2_i = Option.value_exn (int_from_value v2) in
			Int.compare v1_i v2_i
		else if (is_bool_value v1) && (is_bool_value v2) then
			let v1_i = Option.value_exn (bool_from_value v1) in
			let v2_i = Option.value_exn (bool_from_value v2) in
			Bool.compare v1_i v2_i
		else if (is_string_value v1) && (is_string_value v2) then
			let v1_i = Option.value_exn (string_from_value v1) in
			let v2_i = Option.value_exn (string_from_value v2) in
			String.compare v1_i v2_i
		(* Since we are targetting C, also enable bool/int
		   conversions since that what C does.  Note may need
		   to change this for other backends --- or it may just
		   fail before getting here and not matter :) *)
		else if (is_int_value v1) && (is_bool_value v2) then
			let v1_i = Option.value_exn (int_from_value v1) in
			let v1_b = (v1_i <> 0) in (* true if nonzero *)
            let () =
                if options.debug_evaluate_gir then
                    Printf.printf "Partial int bool --- loaded %d and got bool %b\n" v1_i v1_b
                else ()
            in
			let v2_b = Option.value_exn (bool_from_value v2) in
			Bool.compare v1_b v2_b
		else if (is_bool_value v1) && (is_int_value v2) then
			let v1_b = Option.value_exn (bool_from_value v1) in
			let v2_i = Option.value_exn (int_from_value v2) in
			let v2_b = (v2_i <> 0) in
			let () =
                if options.debug_evaluate_gir then
                    Printf.printf "Partial int bool --- loaded %d and got bool %b\n" v2_i v2_b
                else ()
            in
			Bool.compare v1_b v2_b
		(* These last two cases sometimes (rarely) come up.
		I think they have to do with the JSON generation
		sometimes truncating FP values that it prints. *)
		else if (is_int_value v1) && (is_float_value v2) then
			let v1_f = Int.to_float (Option.value_exn (int_from_value v1)) in
			let v2_f = Option.value_exn (float_from_value v2) in
			Float.compare v1_f v2_f

		else if (is_float_value v1) && (is_int_value v2) then
			let v1_f = Option.value_exn (float_from_value v1) in
			let v2_f = Int.to_float (Option.value_exn (int_from_value v2)) in
			Float.compare v1_f v2_f
		else
			raise (EvaluationException ("Can't compare nonints/floats/bools/strings " ^ (synth_value_to_string v1) ^ " and " ^ (synth_value_to_string v2)))
	in
	let () =
        if options.debug_evaluate_gir then
            Printf.printf "Result of comparison was %d (looking at comp mode %s to determine final result)\n" comp_result (binary_comparitor_to_string comp)
            else () in
	let result = BoolV(
	match comp with
	| GreaterThan ->
			comp_result = 1
	| GreaterThanOrEqual ->
			comp_result >= 0
	| LessThan ->
			comp_result = -1
	| LessThanOrEqual ->
			comp_result <= 0
	| Equal ->
			comp_result = 0
	| FloatEqual ->
			(* We need to use the special float euqals
			function , which is approximate.  *)
			let v1_val = Option.value_exn (float_from_value v1) in
			let v2_val = Option.value_exn (float_from_value v2) in
			Utils.float_equal v1_val v2_val
	) in
	let () = if options.debug_evaluate_gir then
        Printf.printf "Result as bool was %s\n" (synth_value_to_string result) else ()
    in
	result
and eval_unary_check ucomp value =
	match ucomp with
	| PowerOfTwo ->
			let value_value = Option.value_exn (int_from_value value) in
			BoolV((value_value land (value_value - 1)) = 0)

let rec eval_variable var valuemap =
	match var with
	| Variable(name) ->
			Hashtbl.find_exn valuemap (gir_name_to_string name)
	| MemberReference(vref, mem_name) ->
			let mem = eval_variable vref valuemap in
			(
			match mem with
			| StructV(name, vtbl) ->
					Hashtbl.find_exn vtbl (gir_name_to_string mem_name)
			| _ -> raise (EvaluationException "Member resulted in non struct result!")
			)
    | Cast(vref, t) ->
            let value = eval_variable vref valuemap in
			(
            match synth_value_cast value t with
            | Some(v) -> v
            (* For casts that fail, we probably just need to
            exapnd the internal casting engine, which is
            as bear-bones as possible when implemented.
            See spec_utils.  *)
            | None ->
                    raise (EvaluationException "Using an unsupported cast: this does not mean the cast would be unsupported in target language.  ")
			)
	| IndexReference(vref, expr) ->
			let ind = eval_expression expr valuemap in
			let ind_value = match ind with
			| Int8V(v) -> v
			| Int16V(v) -> v
			| Int32V(v) -> v
			| Int64V(v) -> v
			| UInt8V(v) -> v
			| UInt16V(v) -> v
			| UInt32V(v) -> v
			| UInt64V(v) -> v
			| _ -> raise (EvaluationException "Invalid index type! (non-int)")
			in
			let arry = eval_variable vref valuemap in
			(
				match arry with
				| ArrayV(elems) -> (
						match List.nth elems ind_value with
                        | None -> raise (EvaluationException "Index out of range")
                        | Some(elem) -> elem
                )
				| _ -> raise (EvaluationException "Invalid array type! (non-array)")
			)
	| Constant(v) -> v

and eval_expression expr valuemap =
	match expr with
	| VariableReference(vref) ->
			eval_variable vref valuemap
	| FunctionCall(fref, vlist) ->
			raise (EvaluationException "Simulation of functions not currently supported!")
	| GIRMap(f, map) ->
			(* This could really easily be implemented.. *)
			raise (EvaluationException "Simulation of GIRMap not currently supported (easy chanes to support it though IMO)")

and eval_conditional options conditional valuemap =
	let () =
        if options.debug_evaluate_gir then
            Printf.printf "Evaling conditional %s\n" (conditional_to_string conditional)
        else () in
	let result = match conditional with
	| Compare(v1, v2, compop) ->
			let v1_value = eval_variable v1 valuemap in
			let v2_value = eval_variable v2 valuemap in
			eval_binary_comp options compop v1_value v2_value
	| Check(v1, unary_comp) ->
			let v1_value = eval_variable v1 valuemap in
			eval_unary_check unary_comp v1_value
	| CondOr(e1, e2) ->
			let e1_result = eval_conditional options e1 valuemap in
			let e2_result = eval_conditional options e2 valuemap in
			(
			match e1_result, e2_result with
			| BoolV(e1), BoolV(e2) ->
					BoolV(e1 || e2)
			| _ -> raise (EvaluationException "Unsupported nonbool type")
			)
	| CondAnd(e1, e2) ->
			let e1_result = eval_conditional options e1 valuemap in
			let e2_result = eval_conditional options e2 valuemap in
			(
			match e1_result, e2_result with
			| BoolV(e1), BoolV(e2) ->
					BoolV(e1 && e2)
			| _ -> raise (EvaluationException "Unsupported nobool type")
			)
	in
	let () =
        if options.debug_evaluate_gir then
            Printf.printf "Result of evaluating conditioanl %s was %s\n" (conditional_to_string conditional) (synth_value_to_string result)
        else () in
	result
