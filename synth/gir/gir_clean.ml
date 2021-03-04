open Core_kernel;;
open Gir;;
open Gir_utils;;

exception UncleanException of string

let rec gir_double_define_clean_internal deftbl gir =
	match gir with
	| Definition(n) ->
			(* See if already defined *)
			let defined =
				Hashtbl.find deftbl (gir_name_to_string n) in
			(* Set defined.  *)
			let () =
				Hashtbl.set deftbl (gir_name_to_string n) true in
			(
			match defined with
				| Some(otherdef) -> EmptyGIR
				| None -> gir
			)
	| Sequence(girs) ->
			Sequence(List.map girs (gir_double_define_clean_internal deftbl))
	| Assignment(_, _) ->
			gir
	| LoopOver(body, indv, maxv) ->
			let defined = Hashtbl.find deftbl (gir_name_to_string indv) in
			let () = Hashtbl.set deftbl (gir_name_to_string indv) true in
			(* Can't have the ind vars defined more than once.
			Could replace this with a clever-er fix, e.g.
			replacing the vars.  *)
			let () = assert (Option.is_none defined) in
			LoopOver(gir_double_define_clean_internal deftbl body, indv, maxv)
	| Expression(_) ->
			gir
	| FunctionDef(name, argslist, body, typmap) ->
			(* Although I don't think we ever reuse variable
			names internally, this is a 'different' function
			so it should have a different cleaning I think. *)
			FunctionDef(name, argslist, gir_double_define_clean body, typmap)
	| Return(_) ->
			gir
	| EmptyGIR -> EmptyGIR

(* Given some GIR, remove things that are defined more than once.
This is produced as an artifact of the generate_gir phase.
*)
and gir_double_define_clean gir =
	let deftbl = Hashtbl.create (module String) in
	gir_double_define_clean_internal deftbl gir

(* Go thorugh al the function calls in the gir and
set their use in table tbl. *)
let rec check_function_calls tbl gir =
    match gir with
    | Definition(_) -> ()
    | Sequence(girs) -> ignore(List.map girs (check_function_calls tbl))
    | Assignment(lval, rval) ->
            check_function_calls_rval tbl rval
    | LoopOver(body, _, maxval) ->
            let () = check_function_calls tbl body in
            check_function_calls_vref tbl maxval
    | Expression(expr) ->
            check_function_calls_expr tbl expr
    | FunctionDef(_, _, _, _) ->
            (* Could do this, but probably don't want to ---
               not sure we want to support functions
               that call other functions yet anyway... *)
            ()
    | Return(_) -> ()
    | EmptyGIR -> ()
and check_function_calls_rval tbl (rval: rvalue) =
    match rval with
        | Expression(expr) ->
                check_function_calls_expr tbl expr
and check_function_calls_vref tbl v =
    match v with
    | Variable(_) -> ()
    | MemberReference(vref, _) ->
            check_function_calls_vref tbl vref
    | IndexReference(vref, expr) ->
            let () = check_function_calls_vref tbl vref in
            check_function_calls_expr tbl expr
and check_function_calls_expr tbl expr =
    match expr with
    | VariableReference(vref) -> check_function_calls_vref tbl vref
    | FunctionCall(fref, vlist) ->
            let () = check_function_calls_fref tbl fref in
            check_function_calls_vlist tbl vlist
    | GIRMap(_, _) ->
            ()
and check_function_calls_vlist tbl vlist =
    match vlist with
    | VariableList(vlist) ->
            ignore(List.map vlist (check_function_calls_vref tbl))
and check_function_calls_fref tbl fref =
    match fref with
    | FunctionRef(Name(n)) ->
            ignore(Hashtbl.set tbl n true)

let remove_unused_fundefs gir fundefs =
    (* Create the fundef hash table: *)
    let funtbl = Hashtbl.create (module String) in
    let () =
        check_function_calls funtbl gir in
    (* Rebuild a fundefs list, but only using the ones
    referenced.  *)
    List.filter_map fundefs (fun fdef ->
        match fdef with
        | FunctionDef(Name(name), _, _, _) ->
                let used = Hashtbl.find funtbl name in
                (
                match used with
                | Some(_) -> Some(fdef)
                | None -> None
                )
        | EmptyGIR -> None
        | _ -> raise (UncleanException ("Unexpected non function in fundefs"))
    )