open Spec_definition;;
open Spec_utils;;
open Core_kernel;;
open Options;;
open Skeleton_definition;;
open Skeleton_utils;;

let rec member x ys =
    match ys with
    | [] -> false
    | y :: ys ->
            (name_reference_equal x y) || (member x ys)

(* This is a pretty shit funciton --- doesn't really
unnest -- don't think it has to though. *)
let rec join_index_nesting_helper indnest =
    match indnest with
    | [] -> []
    | x :: [] -> [x]
    | x :: xs ->
            let subjoin = join_index_nesting_helper xs in
            match x with
            | AnonymousName -> assert false
            | Name(_) -> x :: subjoin
            | StructName(ns) -> ns @ subjoin

let join_index_nesting indnest =
    let nestlist = join_index_nesting_helper indnest in
    match nestlist with
    | [] -> AnonymousName
    | x :: [] -> x
    | xs -> StructName(xs)

let check_all_defined vars binding_list =
    let bound_defins = List.map binding_list.flat_bindings (
        fun bind -> join_index_nesting bind.tovar_index_nesting) in
    ignore(List.map vars (fun v ->
        if member v bound_defins then
            ()
        else
			let () = Printf.printf "Binding list is %s\n" (flat_skeleton_type_binding_to_string binding_list) in
			let () = Printf.printf "Binding list length is %d\n" (List.length binding_list.flat_bindings) in
            let () = Printf.printf "Variable %s not found, only have defines %s" (name_reference_to_string v) (name_reference_list_to_string bound_defins) in
            assert false
    )
    )

let rec get_names typemap classmap x =
    List.concat (List.map x (fun x ->
		let typ = Hashtbl.find_exn typemap x in
		match typ with
		| Struct(nm) ->
				let cmap = Hashtbl.find_exn classmap nm in
				let mems = get_class_fields cmap in
				let tmap = get_class_typemap cmap in
				let nms = get_names tmap classmap mems in
				List.map nms (fun sub_nm ->
					match sub_nm with
					| AnonymousName -> Name(x)
					| Name(_) -> StructName([Name(x); sub_nm])
					| StructName(nms) -> StructName((Name(x)) :: nms)
				)
		| Array(Struct(nm), _) ->
				let cmap = Hashtbl.find_exn classmap nm in
				let mems = get_class_fields cmap in
				let tmap = get_class_typemap cmap in
				let nms = get_names tmap classmap mems in
				List.map nms (fun sub_nm ->
					match sub_nm with
					| AnonymousName -> Name(x)
					| Name(_) -> StructName([Name(x); sub_nm])
					| StructName(nms) -> StructName((Name(x)) :: nms)
				)
		| other ->
				[Name(x)]
	)
	)

let verify_pre classmap (iospec: iospec) (apispec: apispec) pre_binding_list =
    let () = check_all_defined (get_names apispec.typemap classmap apispec.livein) pre_binding_list in
    ()

let verify_post classmap (iospec: iospec) (apispec: apispec) post_binding_list =
    let () = check_all_defined (get_names iospec.typemap classmap iospec.liveout) post_binding_list in
    ()

let verify_skeleton_pairs options classmap (iospec: iospec) (apispec: apispec) pairs =
    ignore(List.map pairs (fun (pre, post) ->
        let () = verify_pre classmap iospec apispec pre in
        let () = verify_post classmap iospec apispec post in
        ()
    )
    )
