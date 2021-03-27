open Core_kernel;;
open Spec_definition;;
open Builtin_conversion_functions;;

(* Keep track of dimvar mappings required for
   certain loops.  We envision that this will
   eventually include more complicated mappings
   than the direct mappings entailed by this.  *)
type one_dim_var_mapping =
	| ExactVarMatch of name_reference * name_reference
	| ConstantMatch of int * name_reference

type dimvar_mapping =
    | DimvarOneDimension of one_dim_var_mapping

(* This is an abstracted type that is used
   for matching different likely compatible types
   together.  It is for the signitures only.  *)
(* The idea is to create an abstraction that represents
   values that are of a similar dimension.
   We have the two base types, SInt and SFloat,
   and then a 'dimension' of SArray.
   Other dimensions would be sensible, e.g. maybe
   a pointer type.  *)
(* Ultimately, we might like to include other
   analysis information along with these types. *)
type skeleton_type =
	(* Base types *)
	| SInt of name_reference
	| SBool of name_reference
	| SFloat of name_reference
and skeleton_dimension_group_type =
	(* Dimension types -- we don't give these names directly.  *)
	| SType of skeleton_type
	| STypes of skeleton_dimension_group_type list
    (* Except for this one :) because it's really a type and has a name *)
	| SArray of name_reference * skeleton_dimension_group_type * dimension_type

type assignment_type =
    (* ie. this is trying to keep track of where the list
       index should go, e.g. complexes[i].real vs complexes.real[i].

       One list for each fromvars
       *)
	| AssignVariable of name_reference list
	| AssignConstant of synth_value

(* These store bindings.  They are both of the form <from> * <to> *)
type single_variable_binding_option_group = {
    (* What parts of the names apply to each fromvar? *)
    fromvars_index_nesting: assignment_type list;
    tovar_index_nesting: name_reference list;
	(* Which dimensions is this assignment valid over? *)
	(* Again, we have one element for each list, but
	provide a list of possible options here.  *)
	valid_dimensions_set: dimvar_mapping list list
}

(* Storing one binding for every variable.  *)
type skeleton_type_binding = {
	bindings: single_variable_binding_option_group list
}

type flat_single_variable_binding = {
    fromvars_index_nesting: assignment_type list;
    tovar_index_nesting: name_reference list;
    valid_dimensions: dimvar_mapping list;
    conversion_function: conversion_functions
}

type flat_skeleton_binding = {
	flat_bindings: flat_single_variable_binding list
}
