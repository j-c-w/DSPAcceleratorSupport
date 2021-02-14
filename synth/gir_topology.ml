open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Gir;;
open Gir_utils;;
open Gir_reduce;;
open Options;;

exception TopologicalSortException of string

type use_def_info_expr = {
	uses: name_reference list;
	(* Expressions can only use, not define or assign.  *)
	(* defs: name_reference list; *)
	(* assigns: name_reference list; *)
}

type use_def_info_rval = {
	uses: name_reference list;
}

type use_def_info_lval = {
	uses: name_reference list;
	assigns: name_reference list;
}

(* Information stored for each gir node.  *)
(* Note that this is not a typical use-def --- it
is a use-def-assign. *)
type use_def_info = {
	uses: name_reference list;
	defs: name_reference list;
	assigns: name_reference list;
	gir: gir;
}

let use_def_to_string ud =
    "GIR: " ^ (gir_to_string ud.gir) ^ "\n" ^
	"Uses: " ^ (name_reference_list_to_string ud.uses) ^ "\n" ^
    "Defs: " ^ (name_reference_list_to_string ud.defs) ^ "\n" ^
    "Assigns: " ^ (name_reference_list_to_string ud.assigns) ^ "\n"

let use_def_list_to_string udl =
	String.concat ~sep:"\n" (List.map udl use_def_to_string)

let rec compute_use_def_assign_for_expr expr: use_def_info_expr =
	match expr with
	| VariableReference(nm) ->
		{
			uses = [nm]
		}
	| ListIndex(expr_base, expr_ind) ->
		let base = compute_use_def_assign_for_expr expr_base in
		let ind = compute_use_def_assign_for_expr expr_ind in
		{
			uses = base.uses @ ind.uses
		}
	(* No dnamically evaluated fcalls right now.  May change
	when we support function calls that are made from objects. *)
	| FunctionCall(_, vars) -> (
		match vars with
		| VariableList(nms) ->
				{
					uses = nms
				}
	)

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
			{
				uses = [];
				assigns = [nm]
			}
	| LIndex(lval, lind) ->
			let index_use_defs = compute_use_def_assign_for_expr lind in
			let var_use_defs = compute_use_def_assign_for_lvalue lval in
			{
				uses = index_use_defs.uses @ var_use_defs.uses;
				assigns = var_use_defs.assigns
			}

let rec get_uses_defining_type typ =
    match typ with
    | Array(subtyp, dim) ->
            let this_dim = match dim with
            | Dimension(nm) -> nm
            | _ -> raise (TopologicalSortException "Don't think this is possible?") in
            this_dim @ (get_uses_defining_type subtyp)
    | _ -> []

let get_uses_defining_variable typemap name =
    (* Compute the variables that get used when defining this.  *)
    get_uses_defining_type (Hashtbl.find_exn typemap (name_reference_to_string name))


let rec compute_use_def_assign_for_node typemap gir =
	match gir with
	| Definition(ndefed) -> {
		uses = get_uses_defining_variable typemap ndefed;
		defs = [ndefed];
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
		(* Filter out the indvar, since that is
		defined and assigned in the loop header.  *)
		let subuses_without_index = List.filter subuses.uses (fun i ->
			not (name_reference_equal i indvar)) in
		let uses = maxvar :: subuses_without_index in
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

let rec member x ys =
	let () = Printf.printf "Checking membership of %s in %s\n" (name_reference_to_string x) (name_reference_list_to_string ys) in
	let res = match ys with
    | [] -> false
    | y :: ys -> (name_reference_member x y) || (member x ys) in
	let () = Printf.printf "Result is %b\n" res in
	res

let rec has_overlap xs ys =
    match xs with
    | [] -> false
    | x :: xs -> (member x ys) || (has_overlap xs ys)

let rec khan_accum (options: options) (girs: use_def_info list) (s: use_def_info list) (defed: name_reference list) (assigned: name_reference list) accum: gir list = match s with
    | [] ->
			let () = if (List.length girs <> 0) then
				let () = Printf.printf "FAILED\n" in
				let () = Printf.printf "Had defed list of %s\n" (name_reference_list_to_string defed) in
				let () = Printf.printf "Had assed list of %s\n" (name_reference_list_to_string assigned) in
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
					let () = Printf.printf "Defined vars %s\n" (name_reference_list_to_string n.defs) in
					let () = Printf.printf "Assed vars %s\n" (name_reference_list_to_string n.assigns) in
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
	let predefined_vars_name_refs = List.map predefed (fun nr -> Name(nr)) in
    let preassigned_vars_name_refs = List.map preassigned (fun nr -> Name(nr)) in
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
