type run_result =
	(* Filename of output JSON. *)
	| Success of string
	| Failure
	(* Perhaps need a timeout? *)
