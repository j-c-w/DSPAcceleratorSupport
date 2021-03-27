open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Skeleton_definition;;
open Skeleton_utils;;
open Builtin_conversion_functions;;
open Range;;
open Utils;;

exception SkeletonRangerException of string

(* Generate a conversion from r1 to r2.  *)
(* We really only do a small subset of this right now ---
handling the general case is obviously exponential,
within an already exponential problem.. *)
let range_conversion r1 r2 =
    let r1_size = range_size r1 in
    let r2_size = range_size r2 in
    if r1_size = r2_size then
        if (range_size_compare r1_size rangeConversionSizeLimit) = -1 then
            permutationConversionOptions r1 r2
        else
            identityConversionFunction r1 r2
    else
        identityConversionFunction r1 r2

(* Various things are implausible, like the fromrange
being lots and lots and the to range being nearly empty.
I suspect a lot more could be done here.  *)
(* There is a lot of nuance we want to avoid here, e.g.
if the user has picked doubles, and the accelerator is
floats, we probably still want to try it.  *)
let range_compat_check from_range to_range =
    let from_size = range_size from_range in
    let to_size = range_size to_range in
    let to_smaller = (range_size_compare from_size to_size) = 1 in
    if to_smaller then
        false
    else
        true

let range_from_fromvars rangemap fromvars =
    match fromvars with
    | [] -> (* no fromvars -- means that this dead-in, so doesnt
			   need to be assigned to. *)
			None
    | [x] ->
            (
            match x with
            | AssignVariable(v) ->
                    let fromvar_name = index_nesting_to_string v in
                    let ranges = Hashtbl.find rangemap fromvar_name in
                    ranges
            | AssignConstant(c) ->
                    range_from_synth_value c
            )
    (* Although the type conceptually supports this, I'm not 100% sure
    that multiple fromvars is really a sane thing to do.  It will
    lead to a lot of blowup if done naively.   Anyway, does warrant
    examination, because I can imagine some very cool usecases
    just need to be careful with hat associated blowup.  Hopefully
    the ranger can do well at reducing it here.  *)
    (* Anywa, I think the rest of this should just work if we go
    down this path. *)
    | x :: xs ->
            raise (SkeletonRangerException "Multiple fromvars aren't yet supported --- will need models of the combination functions used.")

let range_from_tovar rangemap tovar =
    Hashtbl.find rangemap (index_nesting_to_string tovar)

let check_binds from_rangemap to_rangemap (bind: flat_single_variable_binding) =
    (* Currently only support the range detection from a single
       var at this time.  I think that that is the right way
       to go about this --- more vars, and you obviously need
       a combining function call --- now that could (perhaps should?)
       be explored here too, perhaps as a preprocessing pass.
       *)
    let fromvar_range = range_from_fromvars from_rangemap bind.fromvars_index_nesting in
    let tovar_range = range_from_tovar to_rangemap bind.tovar_index_nesting in
    match fromvar_range, tovar_range with
    (* can't check if range is non-existent. *)
    | None, _ -> [bind]
    | _, None -> [bind]
    | Some(frange), Some(trange) ->
            let compatible = range_compat_check frange trange in
            if compatible then
                let conv_fs = range_conversion frange trange in
				List.map conv_fs (fun conv_f -> {
					fromvars_index_nesting = bind.fromvars_index_nesting;
					tovar_index_nesting = bind.tovar_index_nesting;
					valid_dimensions = bind.valid_dimensions;
					conversion_function = conv_f
				})
            else
                (* Not compatible.  *)
				[]

(* This is a pass aimed at 'range-ifying' bindings -- it excludes
   bindings with vastly different valid range bindings, and
   it introduces range conversions between common finite
   set alternatives, e.g. set(-1, 1) and set(0, 1).

   I think this is where we could support power-of-two
   conversions in future, e.g. between set(2, 4, 8, 16...)
   and set(1, 2, 3, 4, 5....)

   This either returns Some(binds) (with ranges)
   or None, if at least one of the binds seemed so range-incompatible
   that it didn't need to happen.
   *)
let rangecheck_binds vbinds from_rangemap to_rangemap =
    let individual_bind_checks =
        List.map vbinds (check_binds from_rangemap to_rangemap) in
    let binds = List.fold individual_bind_checks ~f:(fun binds ->
            fun bindcheck -> match bindcheck, binds with
				| [], _ -> None
				| _, None -> None
                | b, Some(bs) -> Some(b :: bs)
    ) ~init:(Some([])) in
	let binds = match binds with
	| Some(bind) -> bind
	| None -> [] in
	let binds = cross_product binds in
	List.map binds (fun bind -> {
			flat_bindings = bind
		})

(* Note that this can  increase /or/ decrese the number of skeletons
   to check.  *)
let rangecheck_skeletons options (skeletons: flat_skeleton_binding list) from_rangemap to_rangemap =
	List.concat (List.map skeletons (fun skeleton ->
		rangecheck_binds skeleton.flat_bindings from_rangemap to_rangemap
	))
