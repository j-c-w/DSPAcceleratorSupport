open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Options;;
open Utils;;

exception AssignDimensionsException of string

let debug_find_exn tbl name =
	(* Printf.printf "%s%s\n" "Looking for name " name; *)
	Hashtbl.find_exn tbl.variable_map name

let lookup tbl names =
	List.map names (fun name ->
		(name, debug_find_exn tbl name)
	)

let valid_lenvar tbl name =
    match name with
    | Name(varname) ->
			let var = debug_find_exn tbl varname in
			is_integer_type var
    | StructName(sname) ->
			let typ = type_of_name_reference tbl name in
			is_integer_type typ
    | AnonymousName -> raise (AssignDimensionsException "Don't know how to deal with anon name here")

let rec find_possible_dimensions opts typemap all_vars_at_level name : synth_type list=
    (* Only apply to dimensioned types, e.g. arrays.  *)
	let result = match name with
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
				let () =
					if (List.length possible_len_vars) = 0 then
						raise (AssignDimensionsException "Can't find any plausible dimensions for variable")
					else () in
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
	in
	let () = assert ((List.length result) > 0) in
	result

(* If there is a struct being passed, then we need to handle
   each of the subarguments of the struct.  *)
let rec expand_and_wrap_names typemap nms =
	let rec expand_type typemap typ nm =
			match typ with
			| Struct(struct_name) ->
					let struct_info = Hashtbl.find_exn typemap.classmap struct_name in
					(* Recurse and get all the submembers *)
					let subtypmap = {typemap with variable_map = (get_class_typemap struct_info)} in
					let members =
						expand_and_wrap_names subtypmap (get_class_fields struct_info)
					in
					(* We need to prepend this struct's name.  *)
					List.map members (fun mem ->
						name_reference_concat (Name(nm)) mem
					)
			| Pointer(sty) ->
					expand_type typemap sty nm
			| other ->
					(* All other types can just be themselves.  *)
					[Name(nm)]
	in
	List.concat (
		List.map nms (fun nm ->
			let typ = Hashtbl.find_exn typemap.variable_map nm in
			expand_type typemap typ nm
		)
	)

(* Returns a list of types with dimensions assigned to them. *)
let assign_dimensions_to_type opts typemap inptypes typename =
	(* Assign probably dimensions to those types *)
	let () = if opts.debug_assign_dimensions then
		Printf.printf "%s\n" ("Starting to look at variable " ^ typename)
	else () in
    let typ = debug_find_exn typemap typename in
    let restyp = find_possible_dimensions opts typemap inptypes typ in
	let () = assert (List.length restyp > 0) in
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
			let resdata = match metadata with
			| ClassMetadata(cty) ->
					ClassMetadata({ cty with typemap = subtymap; io_typemap = subtymap })
			| StructMetadata(sty) ->
					StructMetadata({ sty with typemap = subtymap; io_typemap = subtymap })
			in
			(cname, resdata)
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
let assign_dimensions (options: options) do_classmaps typemap inps =
	let () = if options.debug_assign_dimensions then
		let () = Printf.printf "Starting to assign dimensions\n" in
        let () = Printf.printf "Variable list is: %s\n" (String.concat ~sep:", " inps) in
        let () = Printf.printf "Typemap entries are: %s\n" (String.concat ~sep:", " (Hashtbl.keys typemap.variable_map)) in
	() else ()
	in
    (* First, do all the inps, or the top level types.  *)
    let top_level_wrapped_names = expand_and_wrap_names typemap inps in
	let () = if options.debug_assign_dimensions then
		let () = Printf.printf "Have the following top level names to choose dimensions from: %s \n"
			(name_reference_list_to_string top_level_wrapped_names)
		in () else () in
	let res_typemaps = List.map inps (assign_dimensions_to_type options typemap top_level_wrapped_names) in
    (* Also preserve the other elements.  *)
    let other_elements = carry_other_elements typemap.variable_map inps in
    let () = if options.debug_assign_dimensions then
        let () = Printf.printf "Executed top level assigns!\n" in
        let () = Printf.printf "Names assigned to were %s\n" (String.concat ~sep:", " inps) in
        ()
	else
        () in
	let result_classmaps =
		if do_classmaps then
			(* Now, do all the classes.  *)
			let classnames = Hashtbl.keys typemap.classmap in
			let res_classmaps = (List.map classnames (fun cname ->
				let metadata = Hashtbl.find_exn typemap.classmap cname in
				let cls_typemap = get_class_typemap metadata in
				let cls_members = get_class_members metadata in
				let sub_typemap = { typemap with variable_map = cls_typemap } in
				let wrapped_cls_members = expand_and_wrap_names sub_typemap cls_members in
				let tps_with_dims = List.map cls_members (assign_dimensions_to_type options sub_typemap wrapped_cls_members) in

				let () = if options.debug_assign_dimensions then
					let () = Printf.printf "For class %s, \n" (cname) in
					let () = Printf.printf "Have options %s\n" (name_reference_list_to_string wrapped_cls_members) in
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
			create_all_classmaps res_classmaps
		else
			[typemap.classmap]
	in
	let result_typemaps = create_all_typemaps (res_typemaps @ other_elements) in
	let () = if options.debug_assign_dimensions then
		Printf.printf "Number of result classmaps is %d, result typemaps is %d\n" (List.length result_classmaps) (List.length result_typemaps)
	else () in
	List.map (List.cartesian_product result_classmaps result_typemaps) (fun (cmap, tmap) ->
		{
			variable_map = tmap;
			classmap = cmap;
			(* The alignment map is unchanged in this pass, so we don't need to recreate it.  *)
			alignment_map = typemap.alignment_map;
			(* We need the dimensions to be assigned ---
			those don't technically modify the 'official'
			types.  *)
			original_typemap = None;
		}
	)
