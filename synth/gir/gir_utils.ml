open Core;;
open Spec_definition;;
open Spec_utils;;
open Gir;;
open Program;;

let gir_name_to_string gname =
    match gname with
    | Name(n) -> n

let gir_name_to_name_reference gname =
	match gname with
	| Name(n) -> name_reference_from_string n

let gir_name_list_to_string gnames =
	String.concat ~sep:", " (List.map gnames ~f:gir_name_to_string)

let gir_name_list_list_to_string gnames =
    String.concat ~sep:";" (List.map gnames ~f:gir_name_list_to_string)

let gir_name_equal n1 n2 =
    match (n1, n2) with
    | Name(n1), Name(n2) -> (String.compare n1 n2) = 0

let rec gir_to_string gir =
	match gir with
    | Definition(nref, escapes, defn_type, initial_value) ->
			let deftype =
				if escapes then "EscapingDefine(UseMalloc)"
				else "Define"
			in
            let value = match initial_value with
            | Some(v) -> " = " ^ (synth_value_to_string v)
            | None -> ""
            in
			deftype ^ " " ^ (gir_name_to_string nref) ^ value
	| IfCond(cond, iftrue, iffalse) ->
			"If(" ^ (conditional_to_string cond) ^ ") {\n" ^
			(gir_to_string iftrue) ^ "\n} else {\n" ^
			(gir_to_string iffalse) ^ "\n}"
	| Sequence(sublist) -> String.concat ~sep:";\n" (List.map sublist ~f:gir_to_string)
	| Assignment(lval, rval) -> (lvalue_to_string lval) ^ " = " ^ (rvalue_to_string rval)
	| LoopOver(body, ind, limit) ->
			"LoopUpTo " ^ (expression_to_string limit) ^
			" indvar " ^ (gir_name_to_string ind) ^
			"{\n" ^ (gir_to_string body) ^ "}"
	| Expression(expr) ->
			(expression_to_string expr)
	| Free(v) ->
			"free " ^ (variable_reference_to_string v)
	| Return(v) ->
			"return " ^ (expression_to_string v)
	| FunctionDef(n, args, body, typmap) ->
			"def " ^
			(gir_name_to_string n) ^ "(" ^ (gir_name_list_to_string args)
			^ ") {\n"  ^ (gir_to_string body) ^ "\n}\n"
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
    | GIRMap(var, values) ->
            (gir_name_to_string var) ^ " match " ^
            (String.concat (List.map values ~f:(fun(f, t) ->
                "| " ^ (synth_value_to_string f) ^ " -> " ^ (synth_value_to_string t) ^ "\n"
            )
            )
            )
and variable_reference_to_string vref =
    match vref with
	| IndexReference(nam, expr) -> (variable_reference_to_string nam) ^
		"[" ^ (expression_to_string expr) ^ "]"
    | Variable(nam) -> gir_name_to_string nam
    | MemberReference(mems, girnam) ->
            (variable_reference_to_string mems) ^ "." ^ (gir_name_to_string girnam)
	| Constant(v) ->
			synth_value_to_string v
	| Cast(v, t) ->
			"(" ^ (synth_type_to_string t) ^ ")" ^ (variable_reference_to_string v)
and varlist_to_string vlist =
	match vlist with
	| VariableList(nams) ->
			String.concat ~sep:"," (List.map nams ~f:variable_reference_to_string)
and function_ref_to_string fref =
	match fref with
	| FunctionRef(nam) ->
			gir_name_to_string nam
and conditional_to_string cond =
	match cond with
	| Compare(v1, v2, comp) ->
			(variable_reference_to_string v1) ^ " " ^
			(binary_comparitor_to_string comp) ^ " " ^
			(variable_reference_to_string v2)
	| Check(v1, comp) ->
			(unary_comparator_to_string comp) ^ " " ^
			(variable_reference_to_string v1)
	| CondOr(e1, e2) ->
			"(" ^ (conditional_to_string e1) ^ ") or (" ^
			(conditional_to_string e2) ^ ")"
	| CondAnd(e1, e2) ->
			"(" ^ (conditional_to_string e1) ^ ") and (" ^
			(conditional_to_string e2) ^ ")"
and binary_comparitor_to_string comp =
	match comp with
	| GreaterThan -> ">"
    | GreaterThanOrEqual -> ">="
	| LessThan -> "<"
    | LessThanOrEqual -> "<="
	| Equal -> "="
    | FloatEqual -> "~="
and unary_comparator_to_string comp =
	match comp with
	| PowerOfTwo -> "PowerOfTwo"

let gir_list_to_string girl =
	String.concat ~sep:"\nGIR:" (List.map girl ~f:gir_to_string)

let gir_list_list_to_string girll =
	String.concat ~sep:"\n=====\n" (List.map girll ~f:gir_list_to_string)

let expression_list_to_string exprl =
	String.concat ~sep:", " (List.map exprl ~f:expression_to_string)

let post_behavioural_to_string (pbp: post_behavioural_program option) =
    match pbp with
    | None -> "None"
    | Some(prog) -> gir_to_string prog.program

let range_program_to_string (rp: range_program option) =
    match rp with
    | None -> "None"
    | Some(prog) -> conditional_to_string prog.condition

let program_to_string (program: program) =
    "Function(" ^ (String.concat ~sep:"," program.funargs) ^ ") {\n" ^
    "Executed if: " ^ (range_program_to_string program.range_checker) ^ "\n" ^
    (gir_to_string program.gir) ^ "\nPostBehavioural: " ^
    (post_behavioural_to_string program.post_behavioural) ^ "\n" ^
    "\n EndFunction (outvars: " ^
    (String.concat ~sep:"," program.liveout) ^ ")\n"

let variable_reference_list_to_string nms =
	String.concat ~sep:", " (List.map nms ~f:variable_reference_to_string)

let variable_reference_option_list_to_string nms =
	String.concat ~sep:", " (List.map nms ~f:(fun n ->
		match n with
		| None -> "None"
		| Some(n) -> variable_reference_to_string n))

let variable_reference_option_to_string opt =
    match opt with
    | None -> "None"
    | Some(n) -> variable_reference_to_string n

let program_list_to_string (programs: program list) =
    String.concat ~sep:"\n\n" (List.map programs ~f:program_to_string)


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
	| Constant(c) ->
			Constant(c)
	| Cast(ref, t) ->
			(* Note that this won't work if the cast is
			ntested  somewhere within the reference... *)
			Cast(build_reference_chain parent ref, t)

(* Do the same thing as build_refernece_chain, but the child
   and the parent can be optional.  *)
let build_reference_chain_optional parent child_option =
	(* let () = Printf.printf "Building with parent %s, child %s" (variable_reference_option_to_string parent) (variable_reference_option_to_string child_option) in *)
	match child_option with
	| Some(c) ->
            (
            match parent with
            | Some(p) -> Some(build_reference_chain p c)
            | None -> Some(c)
            )
	| None -> parent

(* This should be called with the most nested member at
the head of the list.  *)
let rec build_reference_chain_from_reversed_list lst =
	match lst with
	| [] -> None
	| x :: xs ->
			let tail = build_reference_chain_from_reversed_list xs in
			match tail with
			| None -> Some(Variable(x))
			| Some(tl) -> Some(MemberReference(tl, x))

let build_reference_chain_from_list lst =
	build_reference_chain_from_reversed_list (List.rev lst)

let rec get_top_gir_name n =
    match n with
    | Variable(n) -> Variable(n)
    | MemberReference(cls, mem) ->
            get_top_gir_name cls
    | IndexReference(clslist, mem) ->
            get_top_gir_name clslist
    | Constant(c) ->
            Constant(c)
    | Cast(ref, t) ->
            get_top_gir_name ref
