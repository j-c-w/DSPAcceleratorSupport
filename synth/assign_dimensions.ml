open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Options;;

exception AssignDimensionsException of string

let debug_find_exn tbl name =
	(* Printf.printf "%s%s\n" "Looking for name " name; *)
	Hashtbl.find_exn tbl name

let update_typemap typemap name newvalue = (
	(* Get rid of the old binding *)
	Hashtbl.remove typemap name;
	(* Put the new type in.  *)
	Hashtbl.add typemap name newvalue
)

let lookup tbl names =
	List.map names (fun name ->
		(name, debug_find_exn tbl name)
	)

let valid_lenvar tbl name =
    match name with
    | Name(varname) -> (
        let var = debug_find_exn tbl varname in
        match var with
        | Int16 -> true
        | Int32 -> true
        | Int64 -> true
        (* Mabye have a case for array in here? 
           Or a sub-match on classes?*)
        | _ -> false
        )
    | StructName(_) -> raise (AssignDimensionsException "Didn't know how to deal with a struct name here")
    | AnonymousName -> raise (AssignDimensionsException "Don't know how to deal with anon name here")

let rec find_possible_dimensions opts typemap all_vars_at_level name =
    (* Only apply to dimensioned types, e.g. arrays.  *)
    match name with
    | Array(artyp, existing_dims) ->
            (* Recurse and compute any existing dims for any multi
               dimensional arrays.  *)
            let newsubtyp = find_possible_dimensions opts typemap all_vars_at_level artyp in
            (* If this = 1, then the io file has
               already specified this.  Don't override it
               for now, since we don't even support that.  *)
            if existing_dims <> EmptyDimension then
                Array(newsubtyp, existing_dims)
            else
                (* Get all the possible types that are sitting
                   at this level.   May need to modify
                   this eventually to include types that
                   technically sit below this level,
                   e.g. class members/functions. *)
                let possible_len_vars = List.filter all_vars_at_level (valid_lenvar typemap) in
				let () = if opts.debug_assign_dimensions then
					let () = Printf.printf "%s" ("Found " ^ (string_of_int (List.length possible_len_vars)) ^ " vars\n") in
					let () = Printf.printf "%s\n" ("Choosing from " ^ (String.concat ~sep:"," (List.map all_vars_at_level name_reference_to_string))) in
					Printf.printf "%s\n" ("These are possible: " ^ (String.concat ~sep:"," (List.map possible_len_vars name_reference_to_string)))
				else () in
                let newarrtyp = Array(newsubtyp, Dimension(possible_len_vars)) in
                newarrtyp
    | othertype ->
			let () = if opts.debug_assign_dimensions then
				Printf.printf "%s" "Was not an array... \n"
			else () in
			othertype

let wrap_names nms =
    List.map nms (fun nm -> Name(nm))

let assign_dimensions_to_type opts typemap inptypes typename =
	(* Assign probably dimensions to those types *)
	let () = if opts.debug_assign_dimensions then
		Printf.printf "%s\n" ("Starting to look at variable " ^ typename)
	else () in
    let typ = debug_find_exn typemap typename in
    let restyp = find_possible_dimensions opts typemap inptypes typ in
    if typ = restyp then
        ()
    else
        (* Only update if the type has changed. *)
        ignore(update_typemap typemap typename restyp)

(* Assign dimensions to all array types.
   inps is the set of variables to choose
   types from. *)
let assign_dimensions (options: options) (classmap: (string, structure_metadata) Hashtbl.t) typemap inps =
    (* First, do all the inps, or the top level types.  *)
    let top_level_wrapped_names = wrap_names inps in
    ignore(List.map inps (assign_dimensions_to_type options typemap top_level_wrapped_names));
    let () = if options.debug_assign_dimensions then
        let () = Printf.printf "Executed top level assigns!\n" in
        let () = Printf.printf "Names assigned to were %s\n" (String.concat inps) in
        ()
	else
        () in
    (* Now, do all the classes.  *)
    let classnames = Hashtbl.keys classmap in
    ignore(List.map classnames (fun cname ->
        let metadata = debug_find_exn classmap cname in
        let cls_typemap = get_class_typemap metadata in
        let cls_members = get_class_members metadata in
        let wrapped_cls_members = wrap_names cls_members in
		ignore(List.map cls_members (assign_dimensions_to_type options cls_typemap wrapped_cls_members));

		if options.debug_assign_dimensions then
            let () = Printf.printf "Executed length paramter estimate for %s\n" (String.concat ~sep:", " cls_members) in
            ()
		else
			()
	));

	if options.dump_assigned_dimensions then
        let () = Printf.printf "The top-level dimensions are %s\n" (type_hash_table_to_string typemap) in
        Printf.printf "The class-level dimensions are %s\n" (
            String.concat ~sep:"\nNext Class " (List.map classnames (fun name -> name ^ (type_hash_table_to_string (get_class_typemap (Hashtbl.find_exn classmap name))))
        ))
    else
        ()
;;
