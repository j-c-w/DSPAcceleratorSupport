open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Gir;;

let gir_name_to_string gname =
    match gname with
    | Name(n) -> n

let gir_name_equal n1 n2 =
    match (n1, n2) with
    | Name(n1), Name(n2) -> (String.compare n1 n2) = 0

let rec gir_to_string gir =
	match gir with
    | Definition(nref) -> "Define " ^ (gir_name_to_string nref)
	| Sequence(sublist) -> String.concat ~sep:";\n" (List.map sublist gir_to_string)
	| Assignment(lval, rval) -> (lvalue_to_string lval) ^ " = " ^ (rvalue_to_string rval)
	| LoopOver(body, ind, limit) ->
			"LoopUpTo " ^ (variable_reference_to_string limit) ^
			" indvar " ^ (gir_name_to_string ind) ^
			"{\n" ^ (gir_to_string body) ^ "}"
	| Expression(expr) ->
			(expression_to_string expr)
	| EmptyGIR -> ""
and lvalue_to_string lval =
	match lval with
	| LVariable(nam) -> (variable_reference_to_string nam)
and rvalue_to_string rval =
	match rval with
	| Expression(expr) -> (expression_to_string expr)
and expression_to_string expr =
	match expr with
	| VariableReference(nam) -> (variable_reference_to_string nam)
	| FunctionCall(fref, varlist) ->
			(function_ref_to_string fref) ^ "(" ^ (varlist_to_string varlist) ^ ")"
and variable_reference_to_string vref =
    match vref with
	| IndexReference(nam, expr) -> (variable_reference_to_string nam) ^
		"[" ^ (expression_to_string expr) ^ "]"
    | Variable(nam) -> gir_name_to_string nam
    | MemberReference(mems, girnam) ->
            (variable_reference_to_string mems) ^ "." ^ (gir_name_to_string girnam)
and varlist_to_string vlist =
	match vlist with
	| VariableList(nams) ->
			String.concat ~sep:"," (List.map nams variable_reference_to_string)
and function_ref_to_string fref =
	match fref with
	| FunctionRef(nam) ->
			gir_name_to_string nam

let gir_list_to_string girl =
	String.concat ~sep:"\nGIR:" (List.map girl gir_to_string)

let gir_list_list_to_string girll =
	String.concat ~sep:"\n=====\n" (List.map girll gir_list_to_string)

let program_to_string (program: program) =
    "Function(" ^ (String.concat ~sep:"," program.in_variables) ^ ") {\n" ^
    (gir_to_string program.gir) ^ "\n EndFunction (outvars: " ^
    (String.concat ~sep:"," program.out_variables) ^ ")\n"

let variable_reference_list_to_string nms =
	String.concat ~sep:", " (List.map nms variable_reference_to_string)

let variable_reference_option_list_to_string nms =
	String.concat ~sep:", " (List.map nms (fun n ->
		match n with
		| None -> "None"
		| Some(n) -> variable_reference_to_string n))

let gir_name_list_to_string gnames =
	String.concat ~sep:", " (List.map gnames gir_name_to_string)

let program_list_to_string (programs: program list) =
    String.concat ~sep:"\n\n" (List.map programs program_to_string)


(*  Converts, e.g. X.y.z and A.b.c to
X.y.z.A.b.c :) *)
(* Needs to turn e.g. a.e[10] and x.y[i] into
  a.e[10].x.y[i] *)
let rec build_reference_chain parent child =
    match child with
	| Variable(cname) -> MemberReference(parent, cname)
	| MemberReference(Variable(cls), mem) ->
            (* Recursion done :) *)
            MemberReference(
                MemberReference(parent, cls),
                mem
            )
	| IndexReference(Variable(mem), ind_ex) ->
            (* Likewise *)
            IndexReference(
                MemberReference(parent, mem),
                ind_ex
            )
    | MemberReference(clslist, mem) ->
            let subcls = build_reference_chain parent clslist in
            MemberReference(subcls, mem)
    | IndexReference(mem, ind) ->
            let subcls = build_reference_chain parent mem in
            IndexReference(subcls, ind)
