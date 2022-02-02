open Core_kernel;;
open Options;;
open Spec_definition;;
open Spec_utils;;
open Gir;;
open Generate_gir;;
open Gir_clean;;
open Gir_utils;;
open Gir_reduce;;
open Gir_topology;;
open Utils;;
open Generate_io_tests;;

exception GenerateConstantGIRException of string

(* Given  a function, generate a constant-returning body for it. *)
let generate_constant_gir_function (options: options) (typemap: typemap) (iospec: iospec) =
	let funargs = List.map iospec.funargs (fun a -> Name(a)) in
	(* Functions have their own typemaps.  *)
	let funtable = typemap.variable_map in
	let input_types = List.map iospec.funargs (fun arg -> Hashtbl.find_exn typemap.variable_map arg) in
	let returntype = match iospec.returnvar with
	| [] -> Unit
	| [x] ->  Hashtbl.find_exn typemap.variable_map x
	| _ -> assert false
	in
	let _ = Hashtbl.add funtable (iospec.funname) (Fun(input_types, returntype)) in

	match iospec.returnvar with
	| [] ->
			let () = if options.debug_generate_constants then
				Printf.printf "No returnvars, generating empty function\n"
			else () in
			FunctionDef(Name(iospec.funname), funargs, EmptyGIR, funtable)
	| [returnvar] ->
			let () = Printf.printf "Generating constant return value\n" in
		let toposorted_classmap = generate_toposorted_classmap options typemap typemap in
		let rangemap = iospec.rangemap in
        (* TODO --- we should use the value profiles ehre.  *)
		let returnvalue = generate_inputs_for options rangemap (Hashtbl.create (module String)) returnvar returntype returntype toposorted_classmap  in
		(* Now, generate the copy from the stack-allocated variable
		   to the heap allocated varible.  *)

        (* Generate the actual copying code.  *)
        let () = if options.debug_generate_constants then
            let () = Printf.printf "Generating copies from %s to temp_variable\n" returnvar in
            () else () in
        let copies = generate_gir_copies typemap ([returnvar]) (["temp_variable"]) returntype in
		let res_fun = FunctionDef(Name(iospec.funname), funargs,
			Sequence([
				(* Define *)
				Definition(Name(returnvar), true, Some(returntype), None);
				Definition(Name("temp_variable"), false, Some(returntype), Some(returnvalue));
				(* Assign *)
                ] @
                copies @
                [
				(* Return *)
				Return(
					VariableReference(Variable(Name(returnvar)))
				)
			]),
			funtable
		) in
		(* Gir likely has nested subexprs, so remove those.  *)
		reduce_gir options res_fun
	| x :: xs -> raise (GenerateConstantGIRException "Unexpected multiple returnvar")
