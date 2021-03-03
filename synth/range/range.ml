open Range_definition;;
open Random;;

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
