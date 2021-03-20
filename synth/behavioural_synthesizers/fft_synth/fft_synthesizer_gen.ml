open Core_kernel;;
open Fft_synthesizer_definition;;
open Spec_utils;;
open Spec_definition;;
open Options;;

exception CXXHoleError of string

let ind_name_count = ref 0
let generate_ind_name () =
	ind_name_count := !ind_name_count + 1;
	"bi_" ^ (string_of_int !ind_name_count)

let fft_generate_cxx_from_dimension (x: dimension_type) =
    match x with
    | EmptyDimension -> raise (CXXHoleError "Dimension too empty")
    | Dimension([x]) -> (name_reference_to_string x)
    | Dimension(_) -> raise (CXXHoleError "Dimension too full")

let rec generate_cxx_program lenvar_bindings fft_behaviour =
	match fft_behaviour with
	| FSConditional(body, cond) ->
			let condstr = generate_cxx_condition cond in
			let body_str = generate_cxx_program lenvar_bindings body in
			"if (" ^ condstr ^ ") {\n" ^
			body_str ^ "\n}"
	| FSArrayOp(operator, onvar) ->
			let vname = generate_cxx_variable onvar in
			let loop_length = Hashtbl.find_exn lenvar_bindings vname in
			let length_name = fft_generate_cxx_from_dimension loop_length in
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
			let loop_body =
				funname ^ "(" ^ vname ^ ", " ^ length_name ^ ")" in
			loop_body
    | FSSeq(elems) ->
            String.concat ~sep:";\n" (
                List.map elems (generate_cxx_program lenvar_bindings)
            )
	| FSStructureHole -> raise (CXXHoleError "Has a hole")

and generate_cxx_condition cond =
	match cond with
	| FSGreaterThan(v1, v2) ->
			let v1_str = generate_cxx_variable v1 in
			let v2_str = generate_cxx_variable v2 in
			v1_str ^ " > " ^ v2_str
	| FSLessThan(v1, v2) ->
			let v1_string = generate_cxx_variable v1 in
			let v2_string = generate_cxx_variable v2 in
			v1_string ^ " < " ^ v2_string
	| FSPowerOfTwo(v) ->
			let v_string = generate_cxx_variable v in
			(* From https://stackoverflow.com/questions/600293/how-to-check-if-a-number-is-a-power-of-2 *)
			"(" ^ v_string ^ " & (" ^ v_string ^ " - 1 )) == 0"
	| FSConditionalHole ->
			raise (CXXHoleError "Had a hole")

and generate_cxx_variable v =
	match v with
	| FSVariable(n) -> (name_reference_to_string n)
	(* Will have trouble with more complex consts, although
	   we don't actually need those right now.  *)
	| FSConstant(v) -> (synth_value_to_string v)
	| _ -> raise (CXXHoleError "Had a hole")

let generate_program_string (options: options) lenvar_bindings synth_prog =
	match options.target with
	| CXX ->
			generate_cxx_program lenvar_bindings synth_prog
