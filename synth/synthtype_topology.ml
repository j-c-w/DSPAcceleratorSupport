open Spec_definition;;
open Spec_utils;;
open Core_kernel;;

exception STopologyException of string

type synth_type_use_def = {
	name: name_reference;
	dependencies: name_reference list
}

let dep_to_string deps =
	"var " ^ (name_reference_to_string deps.name) ^ " with deps " ^
	(name_reference_list_to_string deps.dependencies)

let dep_list_to_string dlist =
	String.concat ~sep:"\n" (
		List.map dlist dep_to_string
	)

let rec get_dependencies_for classmap typ =
	match typ with
	| Int16 -> []
	| Int32 -> []
	| Int64 -> []
	| Float16 -> []
	| Float32 -> []
	| Float64 -> []
	| Array(tp, dims) ->
			let this_deps = match dims with
			| Dimension([x]) -> x
			| _ -> raise (STopologyException "Not handled ")
			in
			this_deps :: (get_dependencies_for classmap tp)
	| Struct(sname) ->
			(* TODO -- we could do a topo sort of the individual
			fields, but that would only help for very weird
			and IMO unlikely topo chains.  *)
			let metadata = Hashtbl.find_exn classmap sname in
			let subs = get_class_fields metadata in
			let stypedef = get_class_typemap metadata in
			let subtyps = List.map subs (Hashtbl.find_exn stypedef) in
			List.concat (
				List.map subtyps (get_dependencies_for classmap)
			)
	| Unit ->
			[]
	| Fun(f, t) ->
			(* In reality, this could use a whole fuckload
			of variables --- all the ones in a closure.
			Not dealing with lambdas right now though :)
			(phew)
			*)
			[]

let compute_use_defs classmap names typmap =
	List.map names (fun n ->
		let typ = Hashtbl.find_exn typmap (name_reference_to_string n) in
		{
			name = n;
			dependencies = (get_dependencies_for classmap typ)
		}
	)

let split_deps deps =
		let still_has_deps = List.filter deps (fun v -> List.length v.dependencies <> 0) in
		let no_more_deps = List.filter deps (fun v -> List.length v.dependencies = 0) in
		still_has_deps, no_more_deps

let name_ref_def_check n1 n2 =
	not (name_reference_equal n1 n2)

let filter_deps name deps =
	(* TODO --- This should be made more complex to handle
	   fields in classes --- not needed for current targets
	   I think (look at the name_ref_def_check func).  *)
	{
		name = deps.name;
		dependencies = List.filter deps.dependencies (name_ref_def_check name)
	}

let rec synth_khan vars s sorted = match s with
	|  [] -> let () = if (List.length vars <> 0) then
		let () = Printf.printf "FAILED\n" in
		let () = Printf.printf "Had a list of %s left\n" (dep_list_to_string vars) in
		let () = Printf.printf "Had a list of %s done\n" (dep_list_to_string sorted) in
		assert false
	else () in
	sorted
	| n :: ss -> 
			let remaining_vars = List.map vars (filter_deps n.name) in
			let still_has_deps, no_more_deps = split_deps remaining_vars in
			synth_khan still_has_deps (ss @ no_more_deps) (n :: sorted)

let synthtype_toposort classmap snames typemap =
	let deps = compute_use_defs classmap snames typemap in
	let stack, rest = split_deps deps in
	let topo_sorted = synth_khan rest stack [] in
	List.map topo_sorted (fun t -> t.name)
