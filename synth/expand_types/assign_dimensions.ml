open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Options;;
open Utils;;

exception AssignDimensionsException of string

let debug_find_exn tbl name =
	(* Printf.printf "%s%s\n" "Looking for name " name; *)
	Hashtbl.find_exn tbl name

let lookup tbl names =
	List.map names (fun name ->
		(name, debug_find_exn tbl name)
	)

let valid_lenvar tbl name =
    match name with
    | Name(varname) ->
        let var = debug_find_exn tbl varname in
		is_integer_type var
    | StructName(_) -> raise (AssignDimensionsException "Didn't know how to deal with a struct name here")
    | AnonymousName -> raise (AssignDimensionsException "Don't know how to deal with anon name here")

let rec find_possible_dimensions opts typemap all_vars_at_level name : synth_type list=
    (* Only apply to dimensioned types, e.g. arrays.  *)
    match name with
    | Array(artyp, existing_dims) ->
            (* Recurse and compute any existing dims for any multi
               dimensional arrays.  *)
            let newsubtyps = find_possible_dimensions opts typemap all_vars_at_level artyp in
            (* If this = 1, then the io file has
               already specified this.  Don't override it
               for now, since we don't even support that.  *)
            (* Due to my lazyness, this is also called multiple
            times for each variable.  Could just sort out
            at call site, but not likely to be a performance
            issue so... *)
            if not (empty_dimension existing_dims) then
				List.map newsubtyps (fun newsubtyp ->
					Array(newsubtyp, existing_dims)
				)
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
				let possible_len_vars = List.map possible_len_vars (fun lv -> DimVariable(lv)) in
                let newarrtyp =
					List.concat (
						List.map newsubtyps (fun newsubtyp ->
							List.map possible_len_vars (fun lvar ->
								Array(newsubtyp, Dimension(lvar))
							)
						)
					) in
                newarrtyp
    | othertype ->
			let () = if opts.debug_assign_dimensions then
				Printf.printf "%s" "Was not an array... \n"
			else () in
			[othertype]

let wrap_names nms =
    List.map nms (fun nm -> Name(nm))

(* Returns a list of types with dimensions assigned to them. *)
let assign_dimensions_to_type opts typemap inptypes typename =
	(* Assign probably dimensions to those types *)
	let () = if opts.debug_assign_dimensions then
		Printf.printf "%s\n" ("Starting to look at variable " ^ typename)
	else () in
    let typ = debug_find_exn typemap typename in
    let restyp = find_possible_dimensions opts typemap inptypes typ in
	(typename, restyp)

let create_all_typemaps tps =
	let types = List.map tps (fun (x, y) -> y) in
	let names: string list = List.map tps (fun (x, y) -> x) in
	let combinations = cross_product types in
	let newtbls = List.map combinations (fun comb ->
		let newtbl = Hashtbl.create (module String) in
		let _ = List.map (List.zip_exn comb names) (fun (t, n) ->
			let _ = Hashtbl.add newtbl n t in
			()
		) in
		newtbl
	) in
	newtbls

let create_all_classmaps tps =
	let updated_maps = List.map tps (fun (cname, metadata, subtymaps) ->
		List.map subtymaps (fun subtymap ->
			(cname, update_structure_metadata_typemap metadata subtymap)
		)
	)
	in
	let combinations = cross_product updated_maps in
	List.map combinations (fun combination ->
		let classtbl = Hashtbl.create (module String) in
		let _ = List.map combination (fun (name, str) ->
			let result = Hashtbl.add classtbl name str in
			match result with
			| `Ok -> ()
			| `Duplicate -> assert false (* Probably an issue with the producting.  *)
		) in
		classtbl
	)

let carry_other_elements oldtbl expanded_elements =
    let tblkeys = Hashtbl.keys oldtbl in
    List.filter_map tblkeys (fun key ->
        if List.mem expanded_elements key Utils.string_equal then
            None
        else
            Some(key, [Hashtbl.find_exn oldtbl key])
    )


(* Assign dimensions to all array types.
   inps is the set of variables to choose
   types from. *)
let assign_dimensions (options: options) typemap inps =
	let () = if options.debug_assign_dimensions then
		let () = Printf.printf "Starting to assign dimensions\n" in
	() else ()
	in
    (* First, do all the inps, or the top level types.  *)
    let top_level_wrapped_names = wrap_names inps in
	let res_typemaps = List.map inps (assign_dimensions_to_type options typemap.variable_map top_level_wrapped_names) in
    (* Also preserve the other elements.  *)
    let other_elements = carry_other_elements typemap.variable_map inps in
    let () = if options.debug_assign_dimensions then
        let () = Printf.printf "Executed top level assigns!\n" in
        let () = Printf.printf "Names assigned to were %s\n" (String.concat ~sep:", " inps) in
        ()
	else
        () in
    (* Now, do all the classes.  *)
    let classnames = Hashtbl.keys typemap.classmap in
	let res_classmaps = (List.map classnames (fun cname ->
        let metadata = debug_find_exn typemap.classmap cname in
        let cls_typemap = get_class_typemap metadata in
        let cls_members = get_class_members metadata in
        let wrapped_cls_members = wrap_names cls_members in
		let tps_with_dims = List.map cls_members (assign_dimensions_to_type options cls_typemap wrapped_cls_members) in

		let () = if options.debug_assign_dimensions then
            let () = Printf.printf "Executed length paramter estimate for %s\n" (String.concat ~sep:", " cls_members) in
            ()
		else
			()
		in
        (* reconstruct this into a list of typemaps.  *)
		cname, metadata, create_all_typemaps tps_with_dims
	)) in

	let () = if options.dump_assigned_dimensions then
        let () = Printf.printf "The top-level dimensions are %s\n" (type_hash_table_to_string typemap.variable_map) in
        Printf.printf "The class-level dimensions are %s\n" (
            String.concat ~sep:"\nNext Class " (List.map classnames (fun name -> name ^ (type_hash_table_to_string (get_class_typemap (Hashtbl.find_exn typemap.classmap name))))
        ))
    else
        ()
	in
	(* Now, create the product-based list of possible
	typemaps.  *)
	let result_classmaps = create_all_classmaps res_classmaps in
	let result_typemaps = create_all_typemaps (res_typemaps @ other_elements) in
	List.map (List.cartesian_product result_classmaps result_typemaps) (fun (cmap, tmap) ->
		{
			variable_map = tmap;
			classmap = cmap;
		}
	)
