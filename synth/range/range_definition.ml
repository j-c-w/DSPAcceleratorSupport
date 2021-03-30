open Core_kernel;;

(* Could perhaps make this more precise to correspond
   better to the synthtypes.  In fact, I'm not 100% sure
   we really wanted to have this type to start with. *)
type range_type =
	| RangeIntegerType
	| RangeFloatType
	| RangeBoolType
	| RangeArrayType of range_type

(* TODO -- may have to migrate to 64b ints? *)
type range_item =
	| RangeInteger of int
	| RangeFloat of float
	| RangeBool of bool
	| RangeArray of range_type * range_item list

type range_range =
	| RangeRange of range_item * range_item
	| RangeItem of range_item
type range_set = RangeSet of range_range Array.t

type range_value =
	| RInt of int
	| RFloat of float
	| RBool of bool
	| RArray of range_type * range_value list

(* I think this type may end up having to be a bit more
complicated, to capture, e.g. range sizes for floating
point types.  *)
type range_size_t =
	Finite of int
	| Infinite
