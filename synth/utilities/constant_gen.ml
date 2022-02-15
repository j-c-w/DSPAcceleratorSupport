open Core_kernel;;
open Cmdliner;;
open Generate_code;;
open Parse_iospec;;
open Parse_api;;
open Parse_classmap;;
open Spec_definition;;
open Spec_utils;;
open Options;;
open Gir;;
open Program;;
open Generate_constant_gir;;

let () = Printexc.record_backtrace true;;

exception ConstantGenException of string

(* This is a program to generate function bodies.  They return
'real' values, but other than that do nothing.  *)
let main iospec_file output_file seed debug_constant_gen =
	let _ = Random.init seed in
    (* Yes, this is a terrible hack I am 100% going to regret
    because I have no clear understanding why it needs both.  *)
    let options = { default_options with debug_generate_constants = debug_constant_gen; } in
    let iospec, iotypemap, classspec = load_iospec options iospec_file in
	let empty_alignmenttbl = Hashtbl.create (module String) in
	(* Setup the entry for the functio ntuype in the typemap.  *)
	let fromtypes = List.map iospec.funargs (fun a -> Hashtbl.find_exn iotypemap a) in
	let totype = match iospec.returnvar with
	| [] -> Unit
	| [x] -> Hashtbl.find_exn iotypemap x
	| y :: ys -> assert false
	in
	let _ = Hashtbl.add iotypemap iospec.funname (Fun(fromtypes, totype)) in
	(* let () = Printf.printf "Added function type for %s (type %s)\n" (iospec.funname) (synth_type_to_string (Fun(fromtypes, totype))) in *)

	(* Build the actual typemap.  *)
	let rec typemap = {
		variable_map = iotypemap;
		classmap = classspec;
		alignment_map = empty_alignmenttbl;
		(* Some of the udnerlyign functions assume
		the existance of this thing.  *)
		original_typemap = Some(typemap);
	} in
	(* Generate the JSON wrapper: *)
	let constant_gir = generate_constant_gir_function options typemap iospec in
	let main_func = cxx_generate_from_gir options typemap constant_gir in
	let code = otherimports ^ "\n" ^ main_func in
	let () = assert (Filename.check_suffix output_file ".cpp") in
	Out_channel.write_all output_file ~data:code

(* TODO -- use the flag processing also.  *)
let iospec =
	let doc = "IO Spec for the Function" in
	Arg.(required & pos 0 (some string) None & info [] ~docv:"IOSpec" ~doc)

let outfile =
	let doc = "Output file for the function wrap" in
	Arg.(required & pos 1 (some string) None & info [] ~docv:"Outfile" ~doc)

let seed =
	let doc = "Random Seed" in
	Arg.(value & opt int 0 & info ["random-seed"] ~docv:"RandomSeed" ~doc)

let debug_gen_constants =
	let doc = "Debug Generate constants pass " in
	Arg.(value & flag & info ["debug-generate-constants"] ~docv:"DebugGenConst" ~doc)

let info =
	let doc = "Generate Function Bodies" in
	Term.info "FuncGen" ~doc

let args_t = Term.(const main $ iospec $ outfile $ seed $ debug_gen_constants)

let () = Term.exit @@ Term.eval (args_t, info)
