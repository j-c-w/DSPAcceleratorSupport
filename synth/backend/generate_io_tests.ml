open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Options;;
open Yojson;;
open Synthtype_topology;;
open Utils;;
open Range;;
open Program;;
open Range_checker_synth;;

let _ = Random.init 0

exception TypeException of string
exception GenerationFailure

let rec generate_file_numbers upto =
	if upto = 0 then []
	else if upto = 1 then
		["1"]
	else
		(string_of_int upto) :: (generate_file_numbers (upto - 1))

let generate_int_within_range rangemap namestring =
    match Hashtbl.find rangemap namestring with
    | None -> (* It's normal to have no rangemap matches if there
                were no rangevars specified.  *)
            Random.int (1000)
    | Some(range) ->
			(* let () = Printf.printf "Looking at name %s\n" namestring in
			let () = Printf.printf "Has rangemap %s\n" (range_set_to_string range) in
			let () = Printf.printf "Rangeset is emapy is %b\n" (empty_range_set range) in *)
            match random_value_in_range range with
            | RInt(v) -> v
            | _ -> raise (TypeException "Unexpected non-int result to int query")

let generate_uint_within_range rangemap namestring =
	match Hashtbl.find rangemap namestring with
	| None ->
			Random.int (1000)
	| Some(range) ->
			match random_value_in_range range with
			| RInt(v) -> v
			| _ -> raise (TypeException "Unexepcted non-int result to int query")

let generate_bool_within_range rangemap namestring =
	match Hashtbl.find rangemap namestring with
	| None -> (Random.int (1)) = 1
	| Some(range) ->
			raise (TypeException "Range of bools not currently supported! (nee dto add to the rtypes etc.")

let generate_float_within_range rangemap namestring =
    match Hashtbl.find rangemap namestring with
    | None -> (* Ditto above *)
            Random.float 100.0
    | Some(range) ->
            match random_value_in_range range with
            | RFloat(v) -> v
            | _ -> raise (TypeException "Unexepced non-float result to float query")

(* This one assumes that the array has been specified -- the implementation
for generating a random array is below.  *)
let generate_array_from_range rangemap namestring =
	let range = Hashtbl.find_exn rangemap namestring in
	match random_value_in_range range with
	| RArray(_, v) as rarr -> range_value_to_synth_value rarr
	| _ -> raise (TypeException "Unexepected non-array result to the array query")


(* TODO --- Could do with making this a bit more deterministic. *)
let rec generate_inputs_for options rangemap values_so_far name_string t structure_metadata =
    match t with
    (* TODO -- Support negative values.  *)
	| Bool -> BoolV(generate_bool_within_range rangemap name_string)
    | Int16 -> Int16V(generate_int_within_range rangemap name_string)
    | Int32 -> Int32V(generate_int_within_range rangemap name_string)
    | Int64 -> Int64V(generate_int_within_range rangemap name_string)
	| UInt16 -> UInt16V(generate_uint_within_range rangemap name_string)
	| UInt32 -> UInt32V(generate_uint_within_range rangemap name_string)
	| UInt64 -> UInt64V(generate_uint_within_range rangemap name_string)
    | Float16 -> Float16V(generate_float_within_range rangemap name_string)
    | Float32 -> Float32V(generate_float_within_range rangemap name_string)
    | Float64 -> Float64V(generate_float_within_range rangemap name_string)
    | Fun(_, _) -> raise (TypeException "Can't generate types for a fun")
    | Unit -> UnitV
	| Pointer(stype) -> PointerV(generate_inputs_for options rangemap values_so_far name_string stype structure_metadata)
    (* TODO --- Probably need to
       make a distinction between square and non
       square arrays.  *)
    | Array(subtype, dimvar) ->
			if Hashtbl.mem rangemap name_string then
				(* If the array has specified values
				we should use those --- often occurs with things
				that have precomputed constant tables, e.g. twiddle factors.  *)
				generate_array_from_range rangemap name_string
			else
				(* If the name wasn't specified, then we should
				generate an array.  *)
				let dimvar_size = match dimvar with
				| Dimension(dms) -> dms
				| EmptyDimension ->
						raise (TypeException "Can't have empty dimensions!")
				in
				(* If this throws, there's an issue with the topo sorting below --- we
				   expect that the dimvars will have been assigned.  *)
				let arrlen =
						match dimvar_size with
						| DimVariable(dimvar_name) ->
							let wrapper = Hashtbl.find_exn values_so_far (name_reference_to_string dimvar_name) in
							let arrlen = match wrapper with
							| Int16V(v) -> v
							| Int32V(v) -> v
							| Int64V(v) -> v
							| UInt16V(v) -> v
							| UInt32V(v) -> v
							| UInt64V(v) -> v
							| _ ->
									(* probably we could handle this --- just need to have a think
									about what it means. *)
									raise (TypeException "Unexpected list dimension type (non-int) ")
							in
							arrlen
						| DimConstant(c) -> c
				in
				if arrlen < options.array_length_threshold then
					ArrayV(List.map (List.range 0 arrlen) (fun _ -> generate_inputs_for options rangemap values_so_far name_string subtype structure_metadata))
				else
					(* Don't want to try and generate arrays that are too big, because
					it just makes synthesis take forever, espc with
					slower computers.  *)
                    (* let () = Printf.printf "Arrlen is %d, maxlen is %d" (arrlen) (options.array_length_threshold) in *)
					raise (GenerationFailure)
    | Struct(name) ->
            let metadata = Hashtbl.find structure_metadata name in
            (* Get the strcuture metadata *)
            let (members, tmap) = match metadata with
            | Some(ClassMetadata(ctype)) -> (ctype.members, ctype.typemap)
            | Some(StructMetadata(stype)) -> (stype.members, stype.typemap)
            | None -> raise (TypeException("Unbound type " ^ name))
            in
            (* Generate a value for each type in the
              metadata.  *)
            let valuetbl = Hashtbl.create (module String) in
            (* TODO -- maybe need to do something to the values so far in here? *)
            let member_datas = List.map members (fun member -> (generate_inputs_for options rangemap values_so_far (name ^ "." ^ member) (Hashtbl.find_exn tmap member) structure_metadata, member)) in
            (* Now, put those generated values in a map.  *)
            ignore(List.map member_datas (fun (data, m) -> Hashtbl.add valuetbl m data));
            StructV(name, valuetbl)

let rec generate_io_values_worker options rangemap generated_vs vs typemap =
	match vs with
	| [] -> ()
	| x :: xs ->
			let name_string = name_reference_to_string x in
			let typx = Hashtbl.find_exn typemap.variable_map name_string in
            let inputs = generate_inputs_for options rangemap generated_vs name_string typx typemap.classmap in
			let res = Hashtbl.add generated_vs (name_reference_to_string x) inputs in
            let () = assert (match res with | `Ok -> true | _ -> false) in
			(generate_io_values_worker options rangemap generated_vs xs typemap)

let rec generate_io_values options num_tests rangemap livein typemap =
	match num_tests with
	| 0 -> []
	| n ->
        let inputs = ref None in
        let () = while (Option.is_none !inputs) do
            (* This can fail if it happens to generate arrays that
            are too long to test successfully.  *)
            let () = inputs := (try
                let mapping = Hashtbl.create (module String) in
                let () = (generate_io_values_worker options rangemap mapping livein typemap) in
                (* let () = Printf.printf "Generation success\n" in *)
                (Some(mapping))
            with GenerationFailure -> 
                (* let () = Printf.printf "Generation failure\n" in *)
                None
            )
            in ()
        done in
		(Option.value_exn !inputs) :: (generate_io_values options (num_tests - 1) rangemap livein typemap)

let rec value_to_string value =
    let str_value = match value with
	| BoolV(v) -> string_of_bool v
    | Int16V(v) -> string_of_int v
    | Int32V(v) -> string_of_int v
    | Int64V(v) -> string_of_int v
	| UInt16V(v) -> string_of_int v
	| UInt32V(v) -> string_of_int v
	| UInt64V(v) -> string_of_int v
	(* JSON does not support formats of the form 1., so
	we need to append a 0 to make it valid :) *)
    | Float16V(v) -> (string_of_float v) ^ "0"
    | Float32V(v) -> (string_of_float v) ^ "0"
    | Float64V(v) -> (string_of_float v) ^ "0"
    (* Probably not right --- needs to mesh with
    however 'unit' is passed in from the JSON representation.
    Can avoid for now with C++ as main target.  *)
    | UnitV -> "()"
    | ArrayV(vals) ->
            let vals = List.map vals value_to_string in
            "[" ^ (String.concat ~sep:", " vals) ^ "]"
	| PointerV(v) ->
			(value_to_string v) ^ "*"
    | StructV(n, valuetbl) ->
            io_test_to_string (Hashtbl.keys valuetbl) valuetbl
    | FunV(_) ->
            raise (TypeException "Unhandled. ")
	in str_value

and io_test_to_string names values =
    let vals_list = List.map names (fun n ->
        let value = Hashtbl.find_exn values n in
        let value_str = value_to_string value in
        "\"" ^ n ^ "\": " ^ value_str
    ) in
    "{" ^ (String.concat ~sep:",\n" vals_list) ^ "}"

let write_io_tests (options: options) program_number names values =
    let target_folder = options.execution_folder ^ "/" ^ "io/" ^ program_number ^ "/" in
    (* Make the folder.  *)
    let res = Sys.command ("mkdir -p " ^ target_folder) in
    let () = assert (res = 0) in
    let nums = generate_file_numbers (List.length values) in
    (* Convert to json and write out.  *)
	List.map (List.zip_exn nums values) (fun (n, vals) ->
		let targ_file = (target_folder ^ "/" ^ n ^ ".json") in
		let json_str = io_test_to_string names vals in
		let () = Out_channel.write_all targ_file ~data:json_str in
		targ_file
	)

let wrap_nrefs nms =
    List.map nms (fun nm ->
        Name(nm)
    )

let generate_io_tests_for_program options (iospec: iospec) program_number (program: program) =
	let () =
		if options.debug_generate_io_tests then
			Printf.printf "Starting to generate IO tests\n"
		else () in
	(* generate the values for each input. *)
	let num_tests = options.number_of_tests in
    let livein_namerefs = wrap_nrefs iospec.livein in
	(* For generating IO examples (they need to run in the original
	program), we need to use the original program's typemap,
	not the infered typemap.  In practice of course, examples
	generated are entirely equivalent, but casting doesn't
	quite work the same in JSON.  *)
	let io_typemap = match program.typemap.original_typemap with
	| Some(t) -> t
	| None -> program.typemap
	in
	let toposorted_values = synthtype_toposort options io_typemap livein_namerefs in
	let () =
		if options.debug_generate_io_tests then
			Printf.printf "Topo sorted values are %s" (name_reference_list_to_string toposorted_values)
		else () in
	let values = generate_io_values options num_tests program.inputmap toposorted_values io_typemap in
	(* Now, convert those to YoJSON values to be written out.  *)
	let json_files = write_io_tests options program_number iospec.livein values in
	json_files

let generate_io_tests options (iospec: iospec) programs =
	let numbers = generate_file_numbers (List.length programs) in
	Utils.parmap options (fun (number, program) ->
		generate_io_tests_for_program options iospec number program)
        (List.zip_exn numbers programs)
