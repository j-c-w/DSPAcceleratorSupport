open Core_kernel;;
open Range_definition;;
open Range;;
open Spec_utils;;
open Spec_definition;;

exception BuiltinException of string

(* What size of sets should we generate conversion sets for.  *)
let rangeConversionSizeLimit = Finite(4);;

(* These are the types of conversion functions that the
backends know how to generate from.  Adding to here requires
entries in generate_gir etc.  *)
type conversion_functions =
	| IdentityConversion
	| PowerOfTwoConversion
	| Map of synth_type * synth_type * (range_value * range_value) list

let conversion_function_to_string conv_function =
    match conv_function with
    | IdentityConversion -> "IdentityConversion"
	| PowerOfTwoConversion -> "PowerOfTwoConversion"
    | Map(f, t, ftlist) -> "ValueMapConversion(" ^
        (synth_type_to_string f) ^ "->" ^ (synth_type_to_string t) ^ ")"

(* Possible conversion functions --- these are specified
   in their use below.  *)
let forwardMap tfrom vset1 tto vset2 =
	Map(tfrom, tto, List.zip_exn (range_value_set_sort vset1) (range_value_set_sort vset2))

let backwardMap tfrom vset1 tto vset2 =
	(* Reverse one of the lists, but not the other to
	get a backwards map. *)
	Map(tfrom, tto, List.zip_exn (range_value_set_sort vset1) (List.rev (range_value_set_sort vset2)))

(* If the synthesizer asks for the identity
   conversion, give it this.  *)
let identityConversionFunction r1 r2 =
	[IdentityConversion]

(* If the synthesizer says that the sets are
exactly the same size and asks for a permutation
conversion, return these options.  *)
let permutationConversionOptions r1 r2 =
	let value_set_r1 = range_values r1 in
	let value_set_r2 = range_values r2 in
    let fromtype = match range_type r1 with
	| RangeBoolType -> Bool
    | RangeIntegerType -> Int32
    | RangeFloatType -> Float32
	| RangeArrayType(_) ->
			raise (BuiltinException "Array types not supported for conversion")
    in
    let totype = match range_type r2 with
	| RangeBoolType -> Bool
    | RangeIntegerType -> Int32
    | RangeFloatType -> Float32
	| RangeArrayType(_) ->
			raise (BuiltinException "Array types not supported for conversion")
    in
	[
		forwardMap fromtype value_set_r1 totype value_set_r2;
		backwardMap fromtype value_set_r1 totype value_set_r2
	]
