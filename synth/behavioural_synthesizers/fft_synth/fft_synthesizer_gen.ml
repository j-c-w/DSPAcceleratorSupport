open Core_kernel;;
open Fft_synthesizer_definition;;
open Spec_utils;;
open Spec_definition;;
open Options;;
open Gir;;
open Gir_utils;;

exception CXXHoleError of string

let ind_name_count = ref 0
let generate_ind_name () =
	ind_name_count := !ind_name_count + 1;
	"bi_" ^ (string_of_int !ind_name_count)

let fft_generate_gir_from_dimension (x: dimension_type) =
    match x with
    | EmptyDimension -> raise (CXXHoleError "Dimension too empty")
    | Dimension([x]) ->
			(
			match x with
			| DimVariable(x) -> Variable(Name(name_reference_to_string x))
			| DimConstant(c) -> Constant(Int64V(c))
			)
    | Dimension(_) -> raise (CXXHoleError "Dimension too full")

let rec generate_gir_program options lenvar_bindings fft_behaviour =
	match fft_behaviour with
	| FSConditional(body, cond) ->
			let cond = generate_gir_condition cond in
			let body = generate_gir_program options lenvar_bindings body in
			IfCond(cond, body, EmptyGIR)
	| FSArrayOp(operator, onvar) ->
			let vname = generate_gir_variable onvar in
			let loop_length = Hashtbl.find_exn lenvar_bindings (variable_reference_to_string vname) in
			let length_name = fft_generate_gir_from_dimension loop_length in
			let funname =
				match operator with
                (* These are macros defined in the C std lib.  *)
					| FSBitReversal ->
							"BIT_REVERSE"
					| FSNormalize ->
							"ARRAY_NORM"
					| FSDenormalize ->
							"ARRAY_DENORM"
					| FSArrayOpHole -> raise (CXXHoleError "Hole")
			in
			let fref = FunctionRef(Name(funname)) in
			(* These are technically macros, but should
			end up being the smae.   *)
			Expression(FunctionCall(
				fref,
				VariableList([vname; length_name])
			))
    | FSSeq(elems) ->
			Sequence(
				List.map elems (generate_gir_program options lenvar_bindings)
			)
	| FSStructureHole -> raise (CXXHoleError "Has a hole")

and generate_gir_condition cond =
	match cond with
	| FSGreaterThan(v1, v2) ->
			let v1_str = generate_gir_variable v1 in
			let v2_str = generate_gir_variable v2 in
			Compare(v1_str, v2_str, GreaterThan)
	| FSLessThan(v1, v2) ->
			let v1_string = generate_gir_variable v1 in
			let v2_string = generate_gir_variable v2 in
			Compare(v1_string, v2_string, LessThan)
	| FSPowerOfTwo(v) ->
			let v_string = generate_gir_variable v in
			Check(v_string, PowerOfTwo)

	| FSConditionalHole ->
			raise (CXXHoleError "Had a hole")

and generate_gir_variable v =
	match v with
	| FSVariable(n) -> Variable(Name(name_reference_to_string n))
	(* Will have trouble with more complex consts, although
	   we don't actually need those right now.  *)
	| FSConstant(v) -> Constant(v)
	| _ -> raise (CXXHoleError "Had a hole")
