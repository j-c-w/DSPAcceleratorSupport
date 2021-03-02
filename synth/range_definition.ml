open Core_kernel;;

(* TODO -- may have to migrate to 64b ints? *)
type range_item =
	| RangeInteger of int
	| RangeFloat of float
type range_range =
	| RangeRange of range_item * range_item
	| RangeItem of range_item
type range_set = RangeSet of range_range list
