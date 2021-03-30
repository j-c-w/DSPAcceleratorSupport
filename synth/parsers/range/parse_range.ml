(* Parse a range value.  *)
open Core_kernel;;
open Range_definition;;
open Spec_definition;;
open Spec_utils;;
open Yojson.Basic.Util;;

exception TypeCheckException of string

let rec typecheck_item typ item =
	match item with
	| RangeInteger(_) -> is_integer_type typ
	| RangeFloat(_) -> is_float_type typ
	| RangeBool(_) -> is_bool_type typ
	| RangeArray(t, arr) ->
			match typ with
			(* TODO --- could also do a check against dimension here.  *)
			| Array(stype, dims) ->
					List.for_all arr (typecheck_item stype)
			| _ -> false

let typecheck_range_range typ r =
	match r with
	| RangeItem(i) -> typecheck_item typ i
	| RangeRange(f, t) ->
			(typecheck_item typ f) &&
			(typecheck_item typ t)
let typecheck t ast =
	match ast with
	| RangeSet(itms) ->
			Array.for_all (Array.map itms (typecheck_range_range t)) (fun x -> x)

(* We'd really like to handle more complex things, like
functions, e.g. every odd number in this.  *)
let parse_range typ range_string =
	let lexbuf = Lexing.from_string range_string in
	let ast = Rangeparse.t Rangelex.read lexbuf in
    let res = typecheck typ ast in
    if res then
        ast
    else
        raise (TypeCheckException "Range for variable doesn't typecheck")


let load_rangetable classmap typemap range_field =
    let range_tbl = Hashtbl.create (module String) in
    let () = match range_field with
    | `Null -> ()
    | range_json ->
            let ranged_vars = keys range_json in
            let _ = List.map ranged_vars (fun k ->
            let typof = type_of typemap classmap k in
            let r = Hashtbl.add range_tbl k (parse_range typof (range_field |> member k |> to_string)) in
            let () = match r with
            | `Ok -> ()
            | `Duplicate -> raise (TypeCheckException "Duplicate range def!")
            in
            ()
	) in
	() in
    range_tbl

