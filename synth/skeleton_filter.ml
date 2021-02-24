open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Skeleton_definition;;
open Skeleton_utils;;

(* No variables assigned from more than once.  *)
let no_multiplie_cloning_check (skel: skeleton_type_binding) =
    let assigned_from = Hashtbl.create (module String) in
    let () = ignore(
        List.map skel.bindings (fun bind ->
            List.map bind.fromvars_index_nesting (fun indnest ->
                (* Get the indnesst string *)
                let indnest_string = name_reference_to_string (StructName(indnest)) in
                let existing_count = Hashtbl.find assigned_from indnest_string in
                let newvar = match existing_count with
                | None -> 1
                | Some(x) -> x + 1
                in
                Hashtbl.set assigned_from indnest_string newvar
            )
        )
    ) in
    (* make sure that all the variables are used only once.
    Could update this to allow variables being used in
    multidefs to be used more than once, but that is a task
    for anohter day.  *)
    List.for_all (Hashtbl.data assigned_from) (fun v -> v = 1)

(* Check a single skeleton.  *)
let skeleton_check skel =
	(* Don't assign to multiple interface variables
	from the same input variable --- that seems
	unlikely to happen in most contexts.  *)
	no_multiplie_cloning_check skel

(* TODO --- Some filtering here would be a good idea.  *)
let skeleton_pair_check p = true

