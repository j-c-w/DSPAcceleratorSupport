open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Options;;
open Yojson;;
open Synthtype_topology;;
open Utils;;
open Range;;

let _ = Random.init 0

exception TypeException of string

let rec generate_file_numbers upto =
	if upto = 1 then
		["1"]
	else
		(string_of_int upto) :: (generate_file_numbers (upto - 1))

let generate_int_within_range rangemap namestring =
    match Hashtbl.find rangemap namestring with
    | None -> (* It's normal to have no rangemap matches if there
                were no rangevars specified.  *)
            Random.int (1000)
    | Some(range) ->
            match random_value_in_range range with
            | RInt(v) -> v
            | _ -> raise (TypeException "Unexpected non-int result to int query")

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

(* TODO --- Could do with making this a bit more deterministic. *)
let rec generate_inputs_for rangemap values_so_far name_string t structure_metadata =
    match t with
    (* TODO -- Support negative values.  *)
	| Bool -> BoolV(generate_bool_within_range rangemap name_string)
    | Int16 -> Int16V(generate_int_within_range rangemap name_string)
    | Int32 -> Int32V(generate_int_within_range rangemap name_string)
    | Int64 -> Int64V(generate_int_within_range rangemap name_string)
    | Float16 -> Float16V(generate_float_within_range rangemap name_string)
    | Float32 -> Float32V(generate_float_within_range rangemap name_string)
    | Float64 -> Float64V(generate_float_within_range rangemap name_string)
    | Fun(_, _) -> raise (TypeException "Can't generate types for a fun")
    | Unit -> UnitV
    (* TODO --- Probably need to
       make a distinction between square and non
       square arrays.  *)
    | Array(subtype, dimvar) ->
            let dimvar_names = match dimvar with
            | Dimension(dms) -> dms
			| EmptyDimension ->
					raise (TypeException "Can't have empty dimensions!")
            in
            (* If this throws, there's an issue with the topo sorting below --- we
               expect that the dimvars will have been assigned.  *)
            let arrlen_value_wrappers = List.map dimvar_names (
				fun dimvar_name ->
					let wrapper = Hashtbl.find_exn values_so_far (name_reference_to_string dimvar_name) in
					let arrlen = match wrapper with
					| Int16V(v) -> v
					| Int32V(v) -> v
					| Int64V(v) -> v
					| _ ->
							(* probably we could handle this --- just need to have a think
							about what it means. *)
							raise (TypeException "Unexpected list dimension type (non-int) ") in
					arrlen
				)
            in
			(* In the greatest stupid hack of all time, we are
			just going to set the array values to the max
			ov the possible lengths.
			This shouldn't matter for langauges like C unless
			we are considering strings.
			There may be some size issues however.

			Anyway, there is a fix to support that, but it
			will require a more complex io_test input
			format.  Est a few days of work.

			For langauges like python or Java, this should also
			be OK, since there should only be one possible
			array length variable. :) *)
			let arrlen = max_of_int_list arrlen_value_wrappers in
            ArrayV(List.map (List.range 0 arrlen) (fun _ -> generate_inputs_for rangemap values_so_far name_string subtype structure_metadata))
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
            let member_datas = List.map members (fun member -> (generate_inputs_for rangemap values_so_far (name ^ "." ^ member) (Hashtbl.find_exn tmap member) structure_metadata, member)) in
            (* Now, put those generated values in a map.  *)
            ignore(List.map member_datas (fun (data, m) -> Hashtbl.add valuetbl m data));
            StructV(name, valuetbl)

let rec generate_io_values_worker rangemap generated_vs vs typmap classmap =
	match vs with
	| [] -> ()
	| x :: xs ->
			let name_string = name_reference_to_string x in
			let typx = Hashtbl.find_exn typmap name_string in
			let inps = generate_inputs_for rangemap generated_vs name_string typx classmap in
			let res = Hashtbl.add generated_vs (name_reference_to_string x) inps in
            let () = assert (match res with | `Ok -> true | _ -> false) in
			(generate_io_values_worker rangemap generated_vs xs typmap classmap)

let rec generate_io_values num_tests rangemap livein typemap classmap =
	match num_tests with
	| 0 -> []
	| n ->
		let mapping = Hashtbl.create (module String) in
		let () = (generate_io_values_worker rangemap mapping livein typemap classmap) in
		mapping :: (generate_io_values (num_tests - 1) rangemap livein typemap classmap)

let rec value_to_string value =
    let str_value = match value with
	| BoolV(v) -> string_of_bool v
    | Int16V(v) -> string_of_int v
    | Int32V(v) -> string_of_int v
    | Int64V(v) -> string_of_int v
    | Float16V(v) -> string_of_float v
    | Float32V(v) -> string_of_float v
    | Float64V(v) -> string_of_float v
    (* Probably not right --- needs to mesh with
    however 'unit' is passed in from the JSON representation.
    Can avoid for now with C++ as main target.  *)
    | UnitV -> "()"
    | ArrayV(vals) ->
            let vals = List.map vals value_to_string in
            "[" ^ (String.concat ~sep:", " vals) ^ "]"
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

let write_io_tests (options: options) names values =
    let target_folder = options.execution_folder ^ "/" ^ "io" in
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

let generate_io_tests options classmap (iospec: iospec) =
	let () =
		if options.debug_generate_io_tests then
			Printf.printf "Starting to generate IO tests"
		else () in
	(* generate the values for each input. *)
	let num_tests = options.number_of_tests in
    let livein_namerefs = wrap_nrefs iospec.livein in
	let toposorted_values = synthtype_toposort classmap livein_namerefs iospec.typemap in
	let () =
		if options.debug_generate_io_tests then
			Printf.printf "Topo sorted values are %s" (name_reference_list_to_string toposorted_values)
		else () in
	let values = generate_io_values num_tests iospec.rangemap toposorted_values iospec.typemap classmap in
	(* Now, convert those to YoJSON values to be written out.  *)
	let json_files = write_io_tests options iospec.livein values in
	json_files
