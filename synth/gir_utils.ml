open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Gir;;

let rec gir_to_string gir =
	match gir with
    | Definition(nref) -> "Define " ^ (name_reference_to_string nref)
	| Sequence(sublist) -> String.concat ~sep:";\n" (List.map sublist gir_to_string)
	| Assignment(lval, rval) -> (lvalue_to_string lval) ^ " = " ^ (rvalue_to_string rval)
	| LoopOver(body, ind, limit) ->
			"LoopUpTo " ^ (name_reference_to_string limit) ^
			" indvar " ^ (name_reference_to_string ind) ^
			"{\n" ^ (gir_to_string body) ^ "}"
	| Expression(expr) ->
			(expression_to_string expr)
	| EmptyGIR -> ""
and lvalue_to_string lval =
	match lval with
	| LVariable(nam) -> (name_reference_to_string nam)
    | LIndex(lval, ind) ->
            (lvalue_to_string lval) ^ "[" ^
            (expression_to_string ind) ^ "]"
and rvalue_to_string rval =
	match rval with
	| Expression(expr) -> (expression_to_string expr)
and expression_to_string expr =
	match expr with
	| VariableReference(nam) -> (name_reference_to_string nam)
	| ListIndex(nam, expr) -> (expression_to_string nam) ^
		"[" ^ (expression_to_string expr) ^ "]"
	| FunctionCall(fref, varlist) ->
			(function_ref_to_string fref) ^ "(" ^ (varlist_to_string varlist) ^ ")"
and varlist_to_string vlist =
	match vlist with
	| VariableList(nams) ->
			String.concat ~sep:"," (List.map nams name_reference_to_string)
and function_ref_to_string fref =
	match fref with
	| FunctionRef(nam) ->
			name_reference_to_string nam

let gir_list_to_string girl =
	String.concat ~sep:"\nGIR:" (List.map girl gir_to_string)

let gir_list_list_to_string girll =
	String.concat ~sep:"\n=====\n" (List.map girll gir_list_to_string)

let program_to_string (program: program) =
    "Function(" ^ (String.concat ~sep:"," program.in_variables) ^ ") {\n" ^
    (gir_to_string program.gir) ^ "\n EndFunction (outvars: " ^
    (String.concat ~sep:"," program.out_variables) ^ ")\n"

let program_list_to_string (programs: program list) =
    String.concat ~sep:"\n\n" (List.map programs program_to_string)
