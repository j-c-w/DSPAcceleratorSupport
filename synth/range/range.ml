open Core_kernel;;
open Range_definition;;
open Random;;
open Spec_definition;;
open Utils;;

exception RangeError of string

let value_from_range_item item =
	match item with
	| RangeInteger(i) -> RInt(i)
	| RangeFloat(f) -> RFloat(f)

let random_value_from_range_range_in_range r =
	match r with
	| RangeItem(i) -> value_from_range_item i
	| RangeRange(small, large) ->
			let vsmall = value_from_range_item small in
			let vlarge = value_from_range_item large in
			match vsmall, vlarge with
			| RInt(low), RInt(high) ->
					let v = Random.int (high - low) in
					RInt(v + low)
			| RFloat(low), RFloat(high) ->
					let v = Random.float (high -. low) in
					RFloat(v +. low)
			| _, _ ->
					raise (RangeError "Expected a typechecked range before actualy executing it. ")

(* We do a super shitty algorithm for doing this
here, where we just pick random elements from
the set.  Should 100% be more intelligent than this.*)
let random_value_in_range range =
	match range with
	| RangeSet(items) ->
			let n = Random.int (Array.length items) in
			(* Pick a random item and get the thing from that.  *)
			random_value_from_range_range_in_range (Array.get items n);;

let range_size_add x y = match x, y with
    | Infinite, _ -> Infinite
    | _, Infinite -> Infinite
    | Finite(x), Finite(y) -> Finite(x + y)

let range_item_size ritem =
    match ritem with
    | RangeItem(i) -> Finite(1)
    | RangeRange(start, finish) ->
            (
            match start, finish with
            | RangeInteger(s), RangeInteger(f) -> Finite(f - s)
            | RangeFloat(s), RangeFloat(f) ->
                    (* Can we do better than this? *)
                    (* Perhaps by adding to the range size type? *)
                    Infinite
			| _, _ ->
					raise (RangeError ("Type Error"))
            )


(* How many items in this set? *)
let range_size r =
    match r with
    | RangeSet(items) ->
            let item_lengths = Array.map items range_item_size in
            Array.fold item_lengths ~f:range_size_add ~init:(Finite(0))

let range_value_from_item i = match i with
	| RangeInteger(fint) -> RInt(fint)
    | RangeFloat(ffloat) -> RFloat(ffloat)

let range_value_to_item i = match i with
    | RInt(fint) -> RangeInteger(fint)
    | RFloat(ffloat) -> RangeFloat(ffloat)

let range_values_range_range rr =
    match rr with
    | RangeItem(i) -> [range_value_from_item i]
    | RangeRange(f, t) ->
            match f, t with
            | RangeInteger(fint), RangeInteger(toint) ->
                    List.map (int_range fint toint) (fun ival ->
                        RInt(ival))
            | RangeFloat(ffloat), RangeFloat(tofloat) ->
                    raise (RangeError "Can't do value set of floating range (i.e. of infinite window)")
            | _, _ ->
                    raise (RangeError "Type error")

(* Given a range with a finite number of items, generate
a set of values from it.  *)
let range_values rset = match rset with
    | RangeSet(items) ->
            let sub_items = Array.to_list (Array.map items range_values_range_range) in
            (* Warning: this is not uniquified, so needs
            the input ranges to be disjoint.  *)
            List.concat sub_items

(*  Implemented in what I think is the standard OCaml
way of -1 => LT, 0 => EQ, 1 => GT *)
let range_size_compare r1_size r2_size =
    match r1_size, r2_size with
    | Infinite, Infinite -> 0
    | Infinite, _ -> 1
    | _, Infinite -> -1
    | Finite(r1), Finite(r2) ->
            Int.compare r1 r2

let rec range_type_value i = match i with
	| RangeInteger(_) -> RangeIntegerType
	| RangeFloat(_) -> RangeFloatType

and range_type_item i = match i with
	| RangeItem(i) -> range_type_value i
	| RangeRange(f, _) -> range_type_value f

and range_type r = match r with
	| RangeSet(items) ->
			range_type_item (Array.get items 0)

let range_value_set_sort vset =
    (* Perhaps it would be better to keep these sets as arrays? *)
    List.sort vset (fun a -> fun b ->
        match a, b with
        | RInt(a), RInt(b) -> Int.compare a b
        | RFloat(a), RFloat(b) -> Float.compare a b
        | _ -> raise (RangeError "Type error")
    )

let range_value_eq v1 v2 =
	match v1, v2 with
	| RangeInteger(i), RangeInteger(j) -> i = j
	| RangeFloat(i), RangeFloat(j) -> Utils.float_equal i j
	| RangeInteger(_), _ -> false
	| RangeFloat(_), _ -> false

let range_value_in r v =
	match r with
	| RangeItem(i) -> range_value_eq i v
	| RangeRange(f, t) ->
			match f, t, v with
			| RangeInteger(l), RangeInteger(h), RangeInteger(i) ->
					(i >= l) && (i <= h)
			| RangeFloat(l), RangeFloat(h), RangeFloat(i) ->
					((Float.compare l i) <= 0) && ((Float.compare h i) >= 0)
			| _, _, _ -> raise (RangeError "Type error")

let range_compare v1 v2 =
	match v1, v2 with
	| RangeInteger(i1), RangeInteger(i2) ->
			Int.compare i1 i2
	| RangeFloat(f1), RangeFloat(f2) ->
			Float.compare f1 f2
	| _, _ -> raise (RangeError "Type error")

let range_overlap (lower, higher) (lower2, higher2) =
	let new_low = if (range_compare lower lower2) = -1 then
		(* Lower < lower2, so lower2 is the new base *)
		lower2
	else lower
	in
	let new_high = if (range_compare higher higher2) = 1 then
		(* higher > higher2, so higher 2 is the new high *)
		higher2
	else
		higher
	in
	if (range_compare new_low new_high) = 1 then
		(* New_low > new_high, so new range is empty.  *)
		None
	else
		Some(RangeRange(new_low, new_high))

let range_value_to_synth_value rvalue =
	match rvalue with
	(* TODO --- do we need to do something more sane with widths? *)
	| RangeInteger(i) -> Int64V(i)
	| RangeFloat(f) -> Float64V(f)
