(* Parse one of the types.  *)
open Core_kernel;;
open Yojson;;
open Yojson.Basic.Util;;
open Spec_definition;;
open Spec_utils;;
open Options;;

let parse_type options type_string = 
	let lexbuf = Lexing.from_string type_string in
	let ast = Typeparse.t Typelex.read lexbuf in
	let () = if options.debug_parse_type then
		Printf.printf "Loaded string %s into type %s\n" (type_string) (synth_type_to_string ast)
	else () in
    ast
