open Core_kernel;;
open Spec_definition;;
open Gir;;
open Gir_utils;;
open Options;;

let add_all_types_to frommap tomap =
    ignore(Hashtbl.iter_keys frommap (fun (apitypekey: string) ->
        ignore(Hashtbl.add tomap apitypekey (Hashtbl.find_exn frommap apitypekey))))

let build_typemap_for (apispec: apispec) (iospec: iospec) gir =
    let newtbl: (string, synth_type) Hashtbl.t =
        Hashtbl.create (module String) in
    let () = add_all_types_to apispec.typemap newtbl in
    let () = add_all_types_to iospec.typemap newtbl in
    newtbl

let generate_program_for (apispec: apispec) (iospec: iospec) (pre, post) =
	(* Generate the function call to the API.  *)
	let api_funcname = FunctionRef(Name(apispec.funname)) in
	let api_args = VariableList(List.map apispec.livein (fun v -> Name(v))) in
	let funcall = FunctionCall(api_funcname, api_args) in
	(* Ret type --- currently only support one since we are
	   targetting C.  More for functional/tuple languages
	   would not be hard.  *)
	let rettype = Option.map iospec.returnvar (fun v -> LVariable(Name(v))) in
    let body = match rettype with
	| None ->
		(* This is a function call with no explicit
		   return arguments, so just splice the function call in.  *)
		Sequence([pre; Expression(funcall); post])
	| Some(resref) ->
		Sequence([pre;
			Assignment(resref, Expression(funcall));
			post]) in
    let unified_typemap =
        build_typemap_for apispec iospec body in
    {
        in_variables = iospec.livein;
        gir = body;
        out_variables = iospec.liveout;
        typemap = unified_typemap;
    }

(* Given a set of pre/post pairs, fill thsee out into whole programs
   that can be turned into C code.  *)
let generate_programs (opts: options) classmap (iospec: iospec) (api: apispec)
	(conversion_functions: (gir * gir) list)  =
		let result_programs = List.map conversion_functions (generate_program_for api iospec) in
		let () = if opts.dump_generate_program then
			Printf.printf "Generated Programs are: %s\n\n" (
				program_list_to_string result_programs
			)
		else () in
        result_programs
