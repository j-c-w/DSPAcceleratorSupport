open Core_kernel;;
open Spec_definition;;
open Gir;;
open Options;;

let generate_program_for (apispec: apispec) (iospec: iospec) (pre, post) =
	(* Generate the function call to the API.  *)
	let api_funcname = FunctionRef(Name(apispec.funname)) in
	let api_args = VariableList(List.map apispec.livein (fun v -> Name(v))) in
	let funcall = FunctionCall(api_funcname, api_args) in
	(* Ret type --- currently only support one since we are
	   targetting C.  More for functional/tuple languages
	   would not be hard.  *)
	let rettype = Option.map iospec.returnvar (fun v -> LVariable(Name(v))) in
	match rettype with
	| None ->
		(* This is a function call with no explicit
		   return arguments, so just splice the function call in.  *)
		Sequence([pre; Expression(funcall); post])
	| Some(resref) ->
		Sequence([pre;
			Assignment(resref, Expression(funcall));
			post])

(* Given a set of pre/post pairs, fill thsee out into whole programs
   that can be turned into C code.  *)
let generate_programs opts classmap (iospec: iospec) (api: apispec)
	(conversion_functions: (gir * gir) list)  =
		List.map conversion_functions (generate_program_for api iospec)
