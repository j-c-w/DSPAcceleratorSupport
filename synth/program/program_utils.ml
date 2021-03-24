open Core_kernel;;
open Gir;;
open Gir_reduce;;
open Program;;
open Spec_definition;;

exception ProgramException of string

(* Helper function to wrap the accelerator in a conditional.  *)
let rec split_on_condition cond program (sequence_elements: gir list) =
    match sequence_elements with
    | (Expression(FunctionCall(FunctionRef(Name(n)), args)) as fcall) :: rest ->
            (* TODO --- need to handle assignments
            for non-void accelerator functions.  *)
            if (String.compare n program.api_funname) = 0 then
                (* This is the call to the accelerator --- so wrap it in an if statement with a return.  *)
                [
                IfCond(cond,
                    (* If the condition passes, then proceed as normal. *)
                    Sequence(fcall :: rest),
                    (* If false, then call to the user
                    code.  *)
                    Expression(
                    FunctionCall(
                        FunctionRef(
                            Name(program.user_funname)
                        ),
                        VariableList(List.map program.in_variables (fun inv -> Variable(Name(inv))))
                    )
                    )
                )
                ]
            else (* This is some unrelated funcall.  *)
                fcall :: (split_on_condition cond program rest)
    (* TODO --- would like to have a more concrete handlig
    of cases where the user call function is non-void.  *)
    | [] -> raise (ProgramException "Failed to find call to accelerator!")
    | x :: xs -> x :: (split_on_condition cond program xs)

(* Insert the range reduction wrapper if it exists *)
let insert_conditional_call options gir (program: program) =
    match program.range_checker with
    | Some(rcheck) ->
            (
            match gir with
            | Sequence(elems) ->
                    Sequence(split_on_condition rcheck.condition program elems)
            | _ -> raise (ProgramException "Unexpected program structure!")
            )
    | None -> gir

(* This inserts a call into a function to dump the vlaues of the variables
   that are live in to the function.  *)
(* It should be done before the range insertion, because it needs
	to find the function call and insert the dump call so that
	it is called /every/ time.  *)
(* Various backend passes rely on this, and I'm imagining that any
pre-behavioural synthesis will also rely on this. *)
let insert_dump_intermediates_call apispec callname gir program =
	let inserted = ref false in
	let result = match gir with
	| Sequence(elems) ->
			Sequence(List.concat(
				List.map elems (fun elem ->
					match elem with
						| (Expression(FunctionCall(FunctionRef(Name(n)), args))) as fcall ->
							(* TODO --- need to handle assignments
							for non-void accelerator functions.  *)
							if (String.compare n program.api_funname) = 0 then
								let () = inserted := true in
								(* This is the right call --- insert right
								before. *)
								Expression(FunctionCall(FunctionRef(Name(callname)),
									VariableList(
										List.map apispec.livein (fun n ->
											Variable(Name(n))
										)
									)
								)) :: [fcall]
							else
								[fcall]
						| other -> [other]
			)))
	| _ -> raise (ProgramException "Expected outer structure to be a sequence!")
	in
	let () = assert(!inserted) in
	result

let generate_single_gir_body_from options apispec dump_intermediates program =
    (* Merge all the components into a single GIR representation for
       the function body.  *)
	(* WARNING: We don't deal with stuff like returns here right
	now --- those are expected to be dealt with by the appropriate
	backend, due to differences in how different languages handle
	return values (e.g. supporting tuples, vs supporting
	primitivs vs supporting all objects.  *)
	(* Really not 100% sure about what limitations this
	entails. *)
    let post_behavioural_addition = reduce_gir options (Sequence([
		program.gir;
		(match program.post_behavioural with
			| Some(p) -> p.program
			| None -> EmptyGIR
		)
	])) in
	(* If we require the intermediate output, e.g. after the array
	output assignments, then this flag will insert a call to the
	pre accel dump function right before the call to the accelerator.  *)
	let intermediate_dump_addition =
		if dump_intermediates then
			insert_dump_intermediates_call apispec options.pre_accel_dump_function post_behavioural_addition program
		else
			post_behavioural_addition
	in
    insert_conditional_call options intermediate_dump_addition program

let generate_includes_list_from program =
	match program.post_behavioural with
	| Some(p) -> p.includes
	| None -> []
