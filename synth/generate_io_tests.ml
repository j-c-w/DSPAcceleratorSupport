open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Options;;
open Yojson;;
open Synthtype_topology;;

let _ = Random.init 0

exception TypeException of string

let rec generate_file_numbers upto =
	if upto = 1 then
		["1"]
	else
		(string_of_int upto) :: (generate_file_numbers (upto - 1))

(* TODO --- Could do with making this a bit more deterministic. *)
let rec generate_inputs_for values_so_far t structure_metadata =
    match t with
    (* TODO -- Support negative values.  *)
    | Int16 -> Int16V(Random.int (1000))
    | Int32 -> Int32V(Random.int (1000))
    | Int64 -> Int64V(Random.int (1000))
    | Float16 -> Float16V(Random.float (100.0))
    | Float32 -> Float32V(Random.float (100.0))
    | Float64 -> Float64V(Random.float (100.0))
    | Fun(_, _) -> raise (TypeException "Can't generate types for a fun")
    | Unit -> UnitV
    (* TODO --- Probably need to 
       make a distinction between square and non
       square arrays.  *)
    (* TODO --- Need to support array lengths somehow.  *)
	(* TODO --- need to support array lengths consistently
	   across different arrays with the same dimension. *)
    | Array(subtype, dimvar) ->
            let dimvar_name = match dimvar with
            | Dimension([n]) -> n
            | _ -> raise (TypeException "Unexpected complicated dimension!")
            in
            (* If this throws, there's an issue with the topo sorting below --- we
               expect that the dimvars will have been assigned.  *)
            let arrlen_value_wrapper = Hashtbl.find_exn values_so_far (name_reference_to_string dimvar_name) in
            let arrlen = match arrlen_value_wrapper with
            | Int16V(v) -> v
            | Int32V(v) -> v
            | Int64V(v) -> v
            | _ ->
                    (* probably we could handle this --- just need to have a think
                    about what it means. *)
                    raise (TypeException "Unexpected list dimension type (non-int) ")
            in
            ArrayV(List.map (List.range 0 arrlen) (fun _ -> generate_inputs_for values_so_far subtype structure_metadata))
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
            let member_datas = List.map members (fun member -> (generate_inputs_for values_so_far (Hashtbl.find_exn tmap member) structure_metadata, member)) in
            (* Now, put those generated values in a map.  *)
            ignore(List.map member_datas (fun (data, m) -> Hashtbl.add valuetbl m data));
            StructV(name, valuetbl)

let rec generate_io_values_worker generated_vs vs typmap classmap =
	match vs with
	| [] -> ()
	| x :: xs ->
			let typx = Hashtbl.find_exn typmap (name_reference_to_string x) in
			let inps = generate_inputs_for generated_vs typx classmap in
			let res = Hashtbl.add generated_vs (name_reference_to_string x) inps in
            let () = assert (res = `Ok) in
			(generate_io_values_worker generated_vs xs typmap classmap)

let rec generate_io_values num_tests livein typemap classmap =
	match num_tests with
	| 0 -> []
	| n ->
		let mapping = Hashtbl.create (module String) in
		let () = (generate_io_values_worker mapping livein typemap classmap) in
		mapping :: (generate_io_values (num_tests - 1) livein typemap classmap)

let rec value_to_string value =
    let str_value = match value with
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
	let values = generate_io_values num_tests toposorted_values iospec.typemap classmap in
	(* Now, convert those to YoJSON values to be written out.  *)
	let json_files = write_io_tests options iospec.livein values in
	json_files
