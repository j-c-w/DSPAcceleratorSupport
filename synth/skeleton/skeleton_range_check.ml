open Core_kernel;;
open Options;;
open Spec_definition;;
open Spec_utils;;
open Skeleton_definition;;
open Skeleton_utils;;
open Builtin_conversion_functions;;
open Range;;
open Range_definition;;
open Range_checker_synth;;
open Gir_utils;;

exception UnimplementedException

let execute_conversion_on_range conversion inp_ranges =
    match conversion with
    | IdentityConversion ->
            (* Identity is 1 to 1, so must be one assigning var *)
            let () = assert ((List.length inp_ranges) = 1) in
            List.hd_exn inp_ranges
    | Map(fromt, tot, mappairs) ->
            (* Maps are 1 to 1, so must be one assigning var *)
            let () = assert ((List.length inp_ranges) = 1) in
            let inp_range = List.hd_exn inp_ranges in
            (* Create a new range that is the same, but
            has all values that overlap the mapped
            values changed.  *)
            (* Really should unify the range types
               and the synthvalue types into one type.  *)
            match inp_range with
            | RangeSet(range_items) ->
                    let range_items_list = Array.to_list range_items in
                    let result_list = List.concat (List.map range_items_list (fun item ->
                        match item with
                        | RangeItem(i) ->
                                (* See if this is in the map.  *)
                                let res = List.find mappairs (fun (f, t) ->
                                    if range_value_eq (range_value_to_item f) i then
                                        true
                                    else
                                        false
                                ) in
                                let new_item = match res with
                                (* If we found the value, use that. *)
                                | Some((_, new_value)) -> RangeItem(range_value_to_item new_value)
                                (* Otherwise, we didn't use the value.  *)
                                | None -> RangeItem(i)
                                in
                                [new_item]
                        | RangeRange(lower, higher) as range ->
                                (* We are going to split
                                this into a series of ranges
                            and individual values.  e.g.
                            if the map is from 0 to 1 and
                            the range is -5 to 5, then we
                            want to generate -5 to -1,
                            1 and 1 to 5.  *)
                                (* We need to compress ranges after this anyway.  *)
                                let overlapping = List.filter mappairs (fun (f, t) ->
                                    range_value_in range (range_value_to_item f)
                                )
                                in
                                (* Compute the new values.  *)
                                let new_values = List.map overlapping (fun (f, t) ->
                                    RangeItem(range_value_to_item t)
                                ) in
                                (* TODO -- remove the old values --- this overapproximates
                                (which is safe, but unessecary) as is. *)
                                range :: new_values
                    )
                    ) in
                    RangeSet(Array.of_list result_list)

let transform_rangemap_by options map bindings =
    let result_tbl = Hashtbl.create (module String) in
    let () = ignore(List.map bindings.flat_bindings (fun flat_binding ->
        let inputs_count = List.length flat_binding.fromvars_index_nesting in
        let () = if options.debug_range_check then
            let () = Printf.printf "Creating range check for var %s\n" (index_nesting_to_string flat_binding.tovar_index_nesting) in
            let () = Printf.printf "Has %d inputs\n" (inputs_count) in
            ()
        else () in
        if inputs_count = 0 then
            (* If there are no fromvars, this is something that
            is not livein, so won't have any range requirements
            attached to it anyway.  *)
            (* We could assert that here to be honest  *)
            ()
        else
            (* First, get the ranges for each of the input
               variables for this binding.  *)
            let ranges = List.map flat_binding.fromvars_index_nesting (fun fvar ->
                let valuesets: range_set option = Hashtbl.find map (index_nesting_to_string fvar) in
                valuesets
            ) in
            (* Convert the input ranges to output values if they
            exist.  *)
            if List.for_all ranges (Option.is_some) then
                let () = Printf.printf "Number of ranges in is %d\n" (List.length ranges) in
                let ranges = List.filter_map ranges Utils.id in
                let () = Printf.printf "Number of ranges in is %d\n" (List.length ranges) in
                let result_range = execute_conversion_on_range flat_binding.conversion_function ranges in
                let () =
                    Hashtbl.set result_tbl (index_nesting_to_string flat_binding.tovar_index_nesting) result_range
                in
                ()
            else
                (* If any of the inputs to this var have undefined
                   rangemaps, then we can't do anything here.  *)
            ()
    )) in
    result_tbl

let generate_range_check_skeleton options classmap iospec apispec pre_binding =
    (* First, we need to generate what the real input/valid
    ranges are /after/ translation through the binding code. *)
    let transformed_io_rangemap = transform_rangemap_by options iospec.rangemap pre_binding in
    let transformed_io_validmap = transform_rangemap_by options iospec.validmap pre_binding in
    (* Then, use these to call the range gen.  This generates
    some GIR conditions that are going to be used later, not
    any skeleton code --- perhaps they should generate
    some skeleton stuff instead?  Not sure there's any
    benefit to doing that, but it would perhaps
    be cleaner.  *)
    let result = generate_range_check options apispec.livein apispec.validmap transformed_io_rangemap transformed_io_validmap in
    let () = if options.dump_range_check then
        let () = Printf.printf "Generated range check (%s)\n" (match result with
        | None -> "None"
        | Some(cond) -> conditional_to_string cond
        ) in
        ()
    else ()
    in
    result

let generate_range_checks_skeleton options classmap iospec apispec pre_bindings =
    List.map pre_bindings (generate_range_check_skeleton options classmap iospec apispec)