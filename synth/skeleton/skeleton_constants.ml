open Core_kernel;;
open Skeleton_definition;;
open Skeleton_utils;;
open Spec_definition;;
open Spec_utils;;

(* Generate a list of constants to pass in as plausible
assignments.  *)
let generate_plausible_constants_map stypes =
    Hashtbl.create (module String)
