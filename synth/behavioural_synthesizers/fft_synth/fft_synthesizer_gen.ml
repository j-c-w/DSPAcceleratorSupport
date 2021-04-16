open Core_kernel;;
open Fft_synthesizer_definition;;
open Spec_utils;;
open Spec_definition;;
open Options;;
open Gir;;
open Gir_utils;;

exception CXXHoleError of string
exception GenerationError of string

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

let rec generate_gir_program options classmap typemap lenvar_bindings fft_behaviour =
	match fft_behaviour with
	| FSConditional(body, cond) ->
			let cond = generate_gir_condition cond in
			let body = generate_gir_program options classmap typemap lenvar_bindings body in
			IfCond(cond, body, EmptyGIR)
	| FSArrayOp(operator, onvar) ->
			let vname = generate_gir_variable onvar in
			let loop_length = Hashtbl.find_exn lenvar_bindings (variable_reference_to_string vname) in
            let vtype = Hashtbl.find_exn typemap (variable_reference_to_string vname) in
            (* vnames split by stuff that has to come before/after each index -- probably
               should be a list rather than a tuple with options to support nested
               arrays.  *)
            let vnames = match vtype with
                | Array(Float16, _) -> [(vname, None)]
                | Array(Float32, _) -> [(vname, None)]
                | Array(Float64, _) -> [(vname, None)]
                | Array(Struct(name), _) ->
                        (
                        match operator with
                        | FSBitReversal ->
                                (* For a bit-reversal, we shouldn't break it down into subcombonents,
                                because it operates on the high-level array. *)
                                (* TODO -- support multiple dimensions here. *)
                                [(vname, None)]
                        (* For everything else, we need to do one
                        loop for each subcomponent of the array. *)
                        | FSNormalize | FSDenormalize ->
                            let variables = get_class_fields (Hashtbl.find_exn classmap name) in
                            let struct_typmap = get_class_typemap (Hashtbl.find_exn classmap name) in
                            List.map (variables) (fun key ->
                            let typ = Hashtbl.find_exn struct_typmap key in
                            match typ with
                            | Float16 -> (vname, Some(key))
                            | Float32 -> (vname, Some(key))
                            | Float64 -> (vname, Some(key))
                            (* TODO -- other cases here shoulnd't be too hard
                            to support.  *)
                            | _ -> raise (GenerationError "Unexpected type")
                            )
                        | FSArrayOpHole ->
                                raise (CXXHoleError "Can't generate from dsl with hole")
                        )
                | Array(_, _) -> (* TODO --support *)
                        raise (GenerationError "Unimplemented")
                (* Dont know about supporting this one though. *)
                | _ -> raise (GenerationError("Unsupported"))
            in
            let length_name = fft_generate_gir_from_dimension loop_length in
            let calls = List.map vnames (fun (pre, post) ->
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
                let post_index = match post with
                | None -> Variable(Name(""))
                (* The insertion of a dot here is a bit of a ahck
                to make the C macro simpler --- it just does an
                append after the array access and that way
                doesn't have to worry about putting a dot between
                things.  *)
                | Some(n) -> Variable(Name("." ^ n))
                in
                let fref = FunctionRef(Name(funname)) in
                (* These are technically macros, but should
                end up being the smae.   *)
                Expression(FunctionCall(
                    fref,
                    VariableList([vname; post_index; length_name])
                ))
            ) in
            Sequence(calls)
    | FSSeq(elems) ->
			Sequence(
				List.map elems (generate_gir_program options classmap typemap lenvar_bindings)
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
