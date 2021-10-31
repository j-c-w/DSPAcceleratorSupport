open Core_kernel;;
open Options;;
open Spec_definition;;
open Spec_utils;;
open Gir;;
open Generate_gir;;
open Gir_clean;;
open Gir_utils;;
open Gir_topology;;
open Utils;;
open Generate_io_tests;;

exception GenerateConstantGIRException of string

(* Given  a function, generate a constant-returning body for it. *)
let generate_constant_gir_function (options: options) (typemap: typemap) (iospec: iospec) =
	let funargs = List.map iospec.funargs (fun a -> Name(a)) in
	match iospec.returnvar with
	| [] -> FunctionDef(Name(iospec.funname), funargs, EmptyGIR, (Hashtbl.create (module String)))
	| [returnvar] ->
		let returntype = Hashtbl.find_exn typemap.variable_map returnvar in
		let toposorted_classmap = generate_toposorted_classmap options typemap typemap in
		let rangemap = iospec.rangemap in
        (* TODO --- we should use the value profiles ehre.  *)
		let returnvalue = generate_inputs_for options rangemap (Hashtbl.create (module String)) returnvar returntype returntype toposorted_classmap  in
		let input_types = List.map iospec.funargs (fun arg -> Hashtbl.find_exn typemap.variable_map arg) in

		let funtable = Hashtbl.create (module String) in
		let _ = Hashtbl.add funtable (iospec.funname) (Fun(fromtyp, returntype))
		FunctionDef(Name(iospec.funname), funargs,
			Sequence([
				Return(
					VariableReference(Constant(returnvalue))
				)
			]),
			funtable)
		)
	| x :: xs -> raise (GenerateConstantGIRException "Unexpected multiple returnvar")
