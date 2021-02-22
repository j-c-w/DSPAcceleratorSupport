type run_result =
	(* Filename of output JSON. *)
	| RunSuccess of string
	| RunFailure
	(* Perhaps need a timeout? *)
