open Core_kernel;;
open Gir;;
open Gir_reduce;;
open Program;;

let generate_single_gir_body_from options program =
    (* Merge all the components into a single GIR representation for
       the function body.  *)
	(* WARNING: We don't deal with stuff like returns here right
	now --- those are expected to be dealt with by the appropriate
	backend, due to differences in how different languages handle
	return values (e.g. supporting tuples, vs supporting
	primitivs vs supporting all objects.  *)
	(* Really not 100% sure about what limitations this
	entails. *)
	reduce_gir options (Sequence([
		program.gir;
		(match program.post_behavioural with
			| Some(p) -> p.program
			| None -> EmptyGIR
		)
	]))

let generate_includes_list_from program =
	match program.post_behavioural with
	| Some(p) -> p.includes
	| None -> []
