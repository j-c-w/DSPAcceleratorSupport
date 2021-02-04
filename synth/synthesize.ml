open Core_kernel;;
open Spec_definition;;
open Assign_dimensions;;
open Skeleton
open Options;;

exception TypeException of string

(* TODO --- Find a better way to deal with bounds.  *)
let maxarraylength = 100;;
let floatmax = 100.0;;
let intmax = 1000;;

let run_synthesis (opts:options) (classmap: (string, structure_metadata) Hashtbl.t) (iospec: iospec) api =
	(* Assign possible dimension equalities between vector types.  *)
	(* This updates the type ref tables in place, so no reassigns needed.  *)
	let _ = assign_dimensions opts classmap iospec.typemap iospec.livein in
	(* We don't assign dimensions for the API since we assume those
	   can be explicitly listed --- it would not be hard to do if
	   required though. *)
    (* Generate the possible skeletons to consider *)
    let skeleton_pairs = generate_skeleton_pairs opts classmap iospec api in
	if opts.dump_skeletons = true then
		Printf.printf "%s%s\n" "Skeletons are " (skeleton_pairs_to_string skeleton_pairs)
	else
		()
;;

let _ = Random.init 0

(* TODO --- Could do with making this a bit more deterministic. *)
let rec generate_inputs_for t structure_metadata =
    match t with
    (* TODO -- Support negative values.  *)
    | Int16 -> Int16V(Random.int (1000))
    | Int32 -> Int32V(Random.int (1000))
    | Int64 -> Int64V(Random.int (1000))
    | Float16 -> Float16V(Random.float (100.0))
    | Float32 -> Float32V(Random.float (100.0))
    | Float64 -> Float64V(Random.float (100.0))
    | Fun(_, _) -> raise (TypeException "Can't generate types for a fun")
    | Unit -> UnitV
    (* TODO --- Probably need to 
       make a distinction between square and non
       square arrays.  *)
    (* TODO --- Need to support array lengths somehow.  *)
	(* TODO --- need to support array lengths consistently
	   across different arrays with the same dimension. *)
    | Array(subtype, dimvar) ->
            let maxlen = Random.int (maxarraylength) in
            ArrayV(List.map (List.range 0 maxlen) (fun _ -> generate_inputs_for subtype structure_metadata))
    | Struct(name) ->
            let metadata = Hashtbl.find structure_metadata name in
            (* Get the strcuture metadata *)
            let (members, tmap) = match metadata with
            | Some(ClassMetadata(ctype)) -> (ctype.members, ctype.typemap)
            | Some(StructMetadata(stype)) -> (stype.members, stype.typemap)
            | None -> raise (TypeException("Unbound type " ^ name))
            in
            (* Generate a value for each type in the
              metadata.  *)
            let valuetbl = Hashtbl.create (module String) in
            let member_datas = List.map members (fun member -> (generate_inputs_for (Hashtbl.find_exn tmap member) structure_metadata, member)) in
            (* Now, put those generated values in a map.  *)
            ignore(List.map member_datas (fun (data, m) -> Hashtbl.add valuetbl m data));
            StructV(name, valuetbl)
