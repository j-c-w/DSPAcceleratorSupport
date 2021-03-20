type run_result =
	(* Filename of output JSON. *)
	| RunSuccess of string
	| RunFailure
	(* Perhaps need a timeout? *)

type test_result = {
	input: string;
	true_output: string option;
	measured_output: string option;
	passed: bool;
}
