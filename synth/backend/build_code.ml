open Core_kernel;;
open Spec_definition;;
open Options;;

exception BuildingException of string

let rec generate_file_numbers upto =
	if upto = 1 then
		["1"]
	else
		(string_of_int upto) :: (generate_file_numbers (upto - 1))

let get_extension opts =
	match opts.target with
    | CXX -> ".cpp"

let build_code (opts: options) (apispec: apispec) (code: string list) =
	(* Get file numbers *)
	let target_file = opts.execution_folder in
	let file_numbers = generate_file_numbers (List.length code) in
	let extension = get_extension opts in
	let exec_names = if opts.skip_build then
        List.map file_numbers (fun f -> target_file ^ "/" ^ f ^ "_exec")
    else
        (* Make sure the target folder exists *)
        let res = Sys.command ("mkdir -p " ^ target_file) in
        let () = (assert (res = 0)) in
        let compiler_cmd = opts.compiler_cmd in
		let compiler_flags = String.concat ~sep:" " apispec.compiler_flags in
		let additional_flags = String.concat ~sep:" " opts.compiler_flags in
        (* Don't use too many cores --- just thrashing the system with GCC
            instances seems like the wrong approach.  *)
        let () = Printf.printf "Starting!\n" in
        let results, exec_names = List.unzip (Parmap.parmap ~ncores:2 (fun (program_code, program_filename) ->
            (* Write the thing to a file *)
            let filename = target_file ^ "/" ^ program_filename ^ extension in
            let outname = target_file ^ "/" ^ program_filename ^ "_exec" in
            let cmd = compiler_cmd ^ " " ^ additional_flags ^ " " ^ compiler_flags ^ " " ^ filename ^ " -o " ^ outname in
            let () = if opts.debug_build_code then
                let () = Printf.printf "Writing to filename %s\n" filename in
                Printf.printf "Compilng with cmd %s\n" cmd
            else () in
            let () = Out_channel.write_all filename ~data:program_code in
            (* then build the file *)
            let result = Sys.command cmd in
            result, outname
        ) (Parmap.L (List.zip_exn code file_numbers))) in
        let () = ignore(
            List.map results (fun r ->
                assert (r = 0)
            )
        ) in
        exec_names in
    exec_names
