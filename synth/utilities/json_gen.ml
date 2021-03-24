open Core_kernel;;
open Cmdliner;;
open Generate_code;;
open Parse_iospec;;
open Parse_api;;
open Parse_classmap;;
open Options;;
open Gir;;
open Program;;

let () = Printexc.record_backtrace true;;

(* This is a simple program that generates the JSON
wrappers needed for argument processing for a given
interface.

The idea is to pass in the IOSpec that it needs to handle,
this this produces some code that goes around that as a wrapper.
*)
let main iospec_file classspec_file output_file =
    (* Yes, this is a terrible hack I am 100% going to regret
    because I have no clear understanding why it needs both.  *)
	let classspec = load_classmap classspec_file in
    let iospec = load_iospec default_options classspec iospec_file in
    (* TODO --- this needs to be filled with some placeholders
    to indicate that we don't know.  *)
    let emptytbl = Hashtbl.create (module String) in
    (* Most of these values aren't used since we don't gen
    the whole program, just the main.  *)
    let base_program = {
        in_variables = iospec.livein;
        gir = EmptyGIR;
        out_variables = iospec.liveout;
        range_checker = None;
        post_behavioural = None;
        typemap = iospec.typemap;
        returnvar = None;
        user_funname = "TODO";
        generated_funname = "TODO";
        api_funname = "TODO";
        lenvar_bindings = emptytbl;
        fundefs = []
    } in
	(* Generate the JSON wrapper: *)
	let code = otherimports ^ "\n" ^ cxx_main_function default_options classspec iospec false base_program in
	Out_channel.write_all output_file ~data:code

(* TODO -- use the flag processing also.  *)
let iospec =
	let doc = "IO Spec for the Function" in
	Arg.(required & pos 0 (some string) None & info [] ~docv:"IOSpec" ~doc)
let classspec =
	let doc = "Class Specu for the types involved" in
	Arg.(required & pos 1 (some string) None & info [] ~docv:"Classspec" ~doc)

let outfile =
	let doc = "Output file for the function wrap" in
	Arg.(required & pos 2 (some string) None & info [] ~docv:"Outfile" ~doc)

let info =
	let doc = "Generate JSON wrappers" in
	Term.info "JSONGen" ~doc

let args_t = Term.(const main $ iospec $ classspec $ outfile)

let () = Term.exit @@ Term.eval (args_t, info)
