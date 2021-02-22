open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Gir;;
open Gir_utils;;
open Gir_reduce;;
open Options;;

exception TopologicalSortException of string
exception UseDefException of string

type use_def_name =
	| UDName of gir_name
	(* ie. to represent x.y *)
	| UDNameNest of use_def_name * gir_name

(* an expression can only use values, not assign.  *)
type use_def_info_expr = {
	uses: use_def_name list;
}

type use_def_info_variable_reference = {
	uses: use_def_name list;
	(* depending on the location of the expr,
	there might be assigns or might be uses,
	e.g. x[i] = y[i]. (x vs y)
	(x[i] is lvalue, y[i] is expr) *)
	maybe_assigns: use_def_name list;
}

type use_def_info_rval = {
	uses: use_def_name list;
}

type use_def_info_lval = {
	uses: use_def_name list;
	assigns: use_def_name list;
}

(* Information stored for each gir node.  *)
(* Note that this is not a typical use-def --- it
is a use-def-assign. *)
type use_def_info = {
	uses: use_def_name list;
	defs: use_def_name list;
	assigns: use_def_name list;
	gir: gir;
}

let rec ud_name_equal udname1 udname2 =
	match udname1, udname2 with
	| UDName(n1), UDName(n2) ->
			gir_name_equal n1 n2
	| UDNameNest(before, ns), UDNameNest(before2, ns2) ->
			(gir_name_equal ns ns2) &&  (ud_name_equal before before2)
    | _, _ -> false

let rec ud_name_to_string udname =
	match udname with
	| UDName(girname) -> gir_name_to_string girname
	| UDNameNest(udname, girname) ->
			(ud_name_to_string udname) ^ "." ^ (gir_name_to_string girname)

let ud_name_list_to_string uds =
	String.concat ~sep:"," (List.map uds ud_name_to_string)

let use_def_to_string ud =
    "GIR: " ^ (gir_to_string ud.gir) ^ "\n" ^
	"Uses: " ^ (ud_name_list_to_string ud.uses) ^ "\n" ^
    "Defs: " ^ (ud_name_list_to_string ud.defs) ^ "\n" ^
    "Assigns: " ^ (ud_name_list_to_string ud.assigns) ^ "\n"

let use_def_list_to_string udl =
	String.concat ~sep:"\n" (List.map udl use_def_to_string)

let ud_name_from_gir n =
	UDName(n)

let rec gnames_from_ud ud =
	match ud with
	| UDName(n) -> [n]
	| UDNameNest(rest, n) ->
			(gnames_from_ud rest) @ [n]

let rec compute_use_def_assign_for_expr expr: use_def_info_expr =
	match expr with
	| VariableReference(nm) ->
		let vuse_defs = compute_use_def_assign_for_vref nm in
		{
			(* We know that the mauybe assigns
			are actually uses since this is
			an expression.  *)
			uses = vuse_defs.uses @ vuse_defs.maybe_assigns;
		}
	(* No dnamically evaluated fcalls right now.  May change
	when we support function calls that are made from objects. *)
	| FunctionCall(_, vars) -> (
		match vars with
		| VariableList(nms) ->
                let vref_use_defs = List.map nms compute_use_def_assign_for_vref in
				{
                    (* In a variable list, we don't define anything so all maybe
                    assigns are actually uses. :) *)
                    uses = List.concat (List.map vref_use_defs (fun ud -> ud.uses @ ud.maybe_assigns));
				}
	)

and compute_use_def_assign_for_vref vref: use_def_info_variable_reference =
	(* In this case, what is a use and what is a def
	depends on the context.  *)
	match vref with
	| Variable(name) ->
			{
				uses = [];
				maybe_assigns = [ud_name_from_gir name]
			}
	| MemberReference(vref, memname) ->
			let sub_ud = compute_use_def_assign_for_vref vref in
			{
				uses = sub_ud.uses;
				(* Compute the full name for the maybe defs *)
				maybe_assigns = 
					let () = (assert (List.length sub_ud.maybe_assigns = 1)) in
					(* Compute the new gir_name *)
					(* Note that this assert
				and method is what I'm using
				here just because that's what
				I currently expect this
				to do.  There's nothing implicit
				about requigin only one here.  *)
                    [UDNameNest(List.hd_exn sub_ud.maybe_assigns, memname)]

			}
	| IndexReference(variable_reference, expr) ->
			let index_ud = compute_use_def_assign_for_expr expr in
			let refs_ud = compute_use_def_assign_for_vref variable_reference in
			{
				uses = index_ud.uses @ refs_ud.uses;
				maybe_assigns = refs_ud.maybe_assigns
			}

let compute_use_def_assign_for_rvalue (rval: rvalue): use_def_info_rval =
	match rval with
	| Expression(expr) ->
			let expr_uses = compute_use_def_assign_for_expr expr in
			{
				uses = expr_uses.uses
			}
	
let rec compute_use_def_assign_for_lvalue lval: use_def_info_lval =
	match lval with
	| LVariable(nm) ->
			let udefs = compute_use_def_assign_for_vref nm in
			{
				uses = udefs.uses;
				(* since it's in an lvalue, we
				know that the maybe assigns are
				really assigns.  *)
				assigns = udefs.maybe_assigns
			}

let rec get_uses_defining_type typ =
    match typ with
    | Array(subtyp, dim) ->
            let this_dim = match dim with
            | Dimension(nm) ->
                    List.map nm (fun n -> match n with
					| AnonymousName -> raise (TopologicalSortException "No anon names in the typemap!")
					| Name(n) -> UDName(Name(n))
					| StructName(ns) -> raise (TopologicalSortException "Congratualations, you hit the case
					that means you need to do a f*ck of a lot of work fixing the typemaps so that
					they are actually sane and are recursive rather than the weird implicit
					'.' that they use now.  Enjoy!"))
            | _ -> raise (TopologicalSortException "Don't think this is possible?") in
            this_dim @ (get_uses_defining_type subtyp)
    | _ -> []

let get_uses_defining_variable typemap name =
    (* Compute the variables that get used when defining this.  *)
    get_uses_defining_type (Hashtbl.find_exn typemap (gir_name_to_string name))


let rec compute_use_def_assign_for_node typemap gir =
	match gir with
	| Definition(ndefed) -> {
		uses = get_uses_defining_variable typemap ndefed;
		defs = [UDName(ndefed)];
		assigns = [];
		gir = gir
	}
	| Sequence(girs) ->
		let subdefs = List.map girs (compute_use_def_assign_for_node typemap) in
		{
            uses = List.concat (List.map subdefs (fun d -> d.uses));
            defs = List.concat (List.map subdefs (fun d -> d.defs));
            assigns = List.concat (List.map subdefs (fun d -> d.assigns));
            gir = gir;
		}
	| Assignment(lval, rval) ->
		let rval_ud = (compute_use_def_assign_for_rvalue rval) in
		let lval_ud = (compute_use_def_assign_for_lvalue lval) in
		{
			uses = rval_ud.uses @ lval_ud.uses;
			defs = [];
			assigns = lval_ud.assigns;
			gir = gir;
		}
	| LoopOver(body, indvar, maxvar) ->
		let subuses = compute_use_def_assign_for_node typemap body in
        let maxv_uses = compute_use_def_assign_for_vref maxvar in
		(* Filter out the indvar, since that is
		defined and assigned in the loop header.  *)
		let subuses_without_index = List.filter subuses.uses (fun i ->
			not (ud_name_equal i (UDName(indvar)))) in
		let uses = maxv_uses.uses @ maxv_uses.maybe_assigns @ subuses_without_index in
		let defs = subuses.defs in
		let assigns = subuses.assigns in
		{
			uses = uses;
			defs = defs;
			assigns = assigns;
			gir = gir
		}
	| Expression(expr) ->
		let expr_use_defs = compute_use_def_assign_for_expr expr in
		{
			uses = expr_use_defs.uses;
			defs = [];
			assigns = [];
			gir = gir;
		}
	| EmptyGIR ->
		{
			uses = [];
			defs = [];
			assigns = [];
			gir = EmptyGIR
		}


(* Converts a UDName to a list, e.g. x.y -> [x, y] *)
let rec ud_name_to_list x = match x with
	| UDName(x) -> [x]
	| UDNameNest(rest, x) ->
			(ud_name_to_list rest) @ [x]

let rec is_prefix x y =
	match x, y with
	| [], ys -> true
	| xs, [] -> false
	| x :: xs, y :: ys ->
			(gir_name_equal x y) && (is_prefix xs ys)

(*  checks if x is a child of some class y, e.g.
if y is X and x is X.a then it's true. *)
let ud_member x y =
	(* let () = Printf.printf "Checking membership of %s in %s\n" (ud_name_to_string x) (ud_name_to_string y) in *)
	let xlist = ud_name_to_list x in
	let ylist = ud_name_to_list y in
	(* Check if y is a prefix of x *)
	let res = is_prefix ylist xlist in
	(* let () = Printf.printf "Result was %b\n" res in *)
	res

let rec member x ys =
	let res = match ys with
    | [] -> false
    | y :: ys -> (ud_member x y) || (member x ys) in
	res

let rec has_overlap xs ys =
    match xs with
    | [] -> false
    | x :: xs -> (member x ys) || (has_overlap xs ys)

let rec khan_accum (options: options) (girs: use_def_info list) (s: use_def_info list) (defed: use_def_name list) (assigned: use_def_name list) accum: gir list = match s with
    | [] ->
			let () = if (List.length girs <> 0) then
				let () = Printf.printf "FAILED\n" in
				let () = Printf.printf "Had defed list of %s\n" (ud_name_list_to_string defed) in
				let () = Printf.printf "Had assed list of %s\n" (ud_name_list_to_string assigned) in
				let () = Printf.printf "And had GIRs left %s\n" (use_def_list_to_string girs) in
				assert false
				else () in
			accum
    | (n :: ss) ->
            (* Remove any deps than 'n' has.  *)
			let defed = n.defs @ defed in
			let assigned = n.assigns @ assigned in
			let () = if options.debug_gir_topology_sort then
					let () = Printf.printf "Pre-mapping is %s\n" (use_def_list_to_string girs) in
					let () = Printf.printf "Defined vars %s\n" (ud_name_list_to_string n.defs) in
					let () = Printf.printf "Assed vars %s\n" (ud_name_list_to_string n.assigns) in
					() else () in
            (* Now, we need to check if any of these
            nodes were 'freed' anc should be added
            to the s stack.  *)
            let schedulable_girs =
                List.filter girs (fun gir ->
                    (List.for_all gir.uses (fun u -> member u assigned)) &&
                    (List.for_all gir.assigns (fun u -> member u defed))
                ) in
            (* and also calc still girs still
            dependent.  *)
            let dependent_girs =
                List.filter girs (fun gir ->
                    (not (List.for_all gir.uses (fun u -> member u assigned))) ||
                    (not (List.for_all gir.assigns (fun u -> member u defed)))
                ) in
			let () = if options.debug_gir_topology_sort then
			let () = Printf.printf "Adding %d shcdulable girs!\n" (List.length schedulable_girs) in
			let () = Printf.printf "Have %d girs left to shecule and %d girs that are schedulable\n" (List.length dependent_girs) (List.length (schedulable_girs @ ss)) in
			() else () in
            khan_accum options dependent_girs (schedulable_girs @ ss) defed assigned (n.gir :: accum)

(* Topology sort implemented as described in wikipedia *)
let khan (options: options) (gir_uses: use_def_info list) predefed preassed =
    (* Get all the nodes with no dependencies, that
       is, no uses/assigns and put them in a list/stack.  *)
    let s =
        List.filter gir_uses (fun gir ->
            (List.for_all gir.uses (fun u -> member u preassed)) &&
            (List.for_all gir.assigns (fun u -> member u predefed))
                    ) in
    (* Everthing that is not in the 's' stack should
       be in th rest of the nodes to consdier. *)
    let non_starting_girs =
        List.filter gir_uses (fun gir ->
            (not (List.for_all gir.uses (fun u -> member u preassed))) ||
                    (not (List.for_all gir.assigns (fun u -> member u predefed)))
                    ) in
	let () = if options.debug_gir_topology_sort then
		let () = Printf.printf
		"Initial scheduable stack is %s\n" (use_def_list_to_string s) in
		let () = Printf.printf "End of initial stack\n" in
		let () = Printf.printf "Rest of program is %s\n" (use_def_list_to_string non_starting_girs) in
		let () = Printf.printf "End of RoP\n" in
		() else () in
	let reversed_result = khan_accum options non_starting_girs s predefed preassed [] in
	List.rev reversed_result

let topo_sort (options: options) (gir_uses: use_def_info list) (predefed: string list) (preassigned: string list) =
	(* Remove any of the predefined vars from the gir_uses *)
	let () = if options.debug_gir_topology_sort then
		let () = Printf.printf "Predefed vnames is %s" (String.concat ~sep:", " predefed) in
		Printf.printf "Preassed vnames is %s" (String.concat ~sep:", " preassigned)
		else () in
	let predefined_vars_name_refs = List.map predefed (fun nr -> UDName(Name(nr))) in
    let preassigned_vars_name_refs = List.map preassigned (fun nr -> UDName(Name(nr))) in
	let () = if options.debug_gir_topology_sort then
		Printf.printf "Running new TOPO SORT==========\n"
	else () in
    khan options gir_uses predefined_vars_name_refs preassigned_vars_name_refs


(* This is a tripple scheulder.  It needs to make sure  that defines
   come before assings, and that assigns come before uses.  *)
let rec topological_gir_sort (options: options) typemap gir predefed preassed =
	let () = if options.debug_gir_topology_sort then
		Printf.printf "Sorting %s\n" (gir_to_string gir)
	else () in
	let result = match gir with
	 | Sequence(girs) ->
		(* Get the uses/defs for each node. *)
		let use_defs = List.map girs (compute_use_def_assign_for_node typemap) in
		(* Do a topo sort on the use/defs *)
		let sorted_use_defs = topo_sort options use_defs predefed preassed in
		(* Then recreate the sequence in the right order.  *)
		Sequence(sorted_use_defs)
	 | _ -> raise (TopologicalSortException "Can't topo sort a non-sequence") in
	let () = if options.debug_gir_topology_sort then
		Printf.printf "Result is %s\n" (gir_to_string result)
	else ()
	in
	result

(* This ONLY works on SSA-style programs.  *)
(* I.e. /before/ the generate_programs pass, which
generates non-SSA programs.  *)
let rec topological_program_sort opts typemap ?predefed:(predefed=[]) ?preassigned:(preassigned=[]) gir =
    (* Need to reduce any complexities before running the scheduling
    pass.  *)
    let simplified_gir = reduce_gir opts gir in
    let () = reduce_gir_check simplified_gir in
    let sorted_gir =
        topological_gir_sort opts typemap simplified_gir predefed preassigned in
	sorted_gir