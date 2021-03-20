open Core_kernel;;
open Fft_synthesizer;;
open Json_utils;;
open Options;;
open Spec_definition;;
open Generic_sketch_synth;;
open Run_definition;;
open Program;;

exception PostSynthesizerException of string

(* Get the parameters that seem likely to be configuration
   parameters, e.g. not arrays of things, that are
   values, and nothing liveout, since that'll also
   be a value --- just to be passed to the synthesizer
   to allow it to make decisions.  *)
(* TODO -- make this the APISpec, it will be easier
to get heuristics for probably.  It's IOspec due
to ease of access to parameters--- for the APISpec,
we need to make sure the parameters appear in the output
Json files.  *)
let configuration_parameters_for (iospec: iospec) (apispec: apispec) =
	let deadout = Utils.set_difference (Utils.string_equal) iospec.livein iospec.liveout in
	let config_deadout = List.filter deadout (fun deadvar ->
		let typ = Hashtbl.find_exn iospec.typemap deadvar in
		match typ with
		| Int16 -> true
		| Int32 -> true
		| Int64 -> true
		(* I think this should be domain-specific, or
		perhaps range specific?  E.g. in the FFT domain,
		individual floats are highly likely to be configuration
		parameters. *)
		| Float16 -> true
		| Float32 -> true
		| Float64 -> true
		| Unit -> false
		| Array(_, _) -> false
		(* FIXME --- need to probably support passing
		this if it has configuration parameters within it. *)
		(* TODO --- ^^ that is probably not the best fix,
		what we should really do is traverse the struct, stop
		at any arrays, and just pass in refs to the
		individual components of the struct we thing are config
		parameters (e.g. array lengths).  *)
		| Struct(name) -> false
		| Fun(_, _) -> false
	) in
	config_deadout

let create_unified_typemap h1 h2 =
    let result = Hashtbl.create (module String) in
    let _ = List.map (Hashtbl.keys h1) (fun key ->
        Hashtbl.add result key (Hashtbl.find_exn h1 key)
    ) in
    let _ = List.map (Hashtbl.keys h2) (fun key ->
        let res = Hashtbl.add result key (Hashtbl.find_exn h2 key) in
        match res with
        | `Ok -> ()
        (* This isn't a fundmental error, more a lazy cannae be fucked to deal with this
        right now.  *)
        | `Duplicate -> raise (PostSynthesizerException "Error: name clash between IO and API")
    ) in
    result

(* The iofiles are pairs of files, where one represents the inputs
   and the other represents the outputs.  We need to get
   the configuration parameters from the right one.  *)
let post_synthesis_io_pairs options apispec iospec iofiles configuration_parameters =
	let result = List.filter_map iofiles (fun (test_results: test_result) ->
		let () = if options.debug_post_synthesis then
			Printf.printf "Loading inputs from file %s\n" (test_results.input)
		else () in
		(* Load variable assignments from the outp, and the true outp. *)
		(* Load only the values in configuration paramters here: *)
		let inp_values = load_value_map_from test_results.input in
		match test_results.measured_output, test_results.true_output with
		| Some(measured_outps), Some(true_outps) ->
			let () = if options.debug_post_synthesis then
				let () = Printf.printf "Loading true outputs from file %s\n" (measured_outps) in
				Printf.printf "Loading gened outputs from file %s\n" (true_outps)
			else () in
			let outp_values = load_value_map_from measured_outps in
			let true_outp_values = load_value_map_from true_outps in
			let _ = List.map configuration_parameters (fun v ->
				Hashtbl.add outp_values v (Hashtbl.find_exn inp_values v)
			) in
			(* ((inputs, configs), required results) *)
			Some({
				input=outp_values;
				output=true_outp_values
			})
		| None, None ->
			(* If they both failed, we just don't need to
			do anything.  *)
			None
		| _, _ ->
			(* If only one failed, there is an issue with
				the valid range detector.  Can't solve that
				here, so just skip this test (This seems
				like it might be a bad idea...) *)
			None
	) in result

let synthesize_post (options: options) classmap (iospec: iospec) (apispec: apispec) (program: program) io_files =
	(* This synthesizes a program based on the IO files,
	   and the io/apispecs that tries to bridge
	   the gap between the finished outputs and
	   the outputs required by the function.

	   This should be available to multiple backend
	   synthesizers, in partiuclar I think that
	   Feser 2015 might work well.  Critically,
	   post-synthesis is a /scalable/ problem --- it
	   can be done independently of the code that
	   comes before.   Pre-syntheses is a slightly
	   more challenging problem, as the underlying
	   synthesizer won't /know/ what the IO pairs
	   are until after it can generate some output
	   for a given input.  *)

	(* For now, use the FFT synth, which knows domain-specific
	answers to likely incompatibility problems
	(scaling/inverse scaling, bit reversal). *)

	(* The inputs to the synthesizer are in pairs ---
	   they must cover both the /configuration settings/
	   and the I/O values --- some things only need
	   to happen in certain configurations, e.g.
	   scaling in FFT/IFFT.  *)
	(* We want to use the APIspec for configuration parameters
	because it is easier to get heuristic support for the
	API spec --- impossible to get good heuristics for the user
	code. *)
    (* We use the IOSpec since we have values for that
    --- need to modify the first round of IO analysis
    to get values for the APISpec inputs here.  *)
	(* TODO -- need to fix issue when there is a name clash
	between input code and real code.  *)
	let configuration_parameters = configuration_parameters_for iospec apispec in
    let () = if options.debug_post_synthesis then
        Printf.printf "Configuration parameters detected is: %s\n" (
            String.concat ~sep:", " (configuration_parameters)
        )
    else () in
	(* The type of the function we want to synthesize is:
		(iospec.liveout * config_params) -> iospec.liveout *)
	(* Hash type ((synth_value list * synth_value list) * synth_value list) list *)
    let io_pairs = post_synthesis_io_pairs options apispec iospec io_files configuration_parameters in
    (* create the unified typemaps.  *)
    (* TODO _-- REALLy need to handle name clashes --- perhaps
       by prefixing things? *)
    let names = List.map (configuration_parameters @ iospec.liveout) (fun n -> Name(n)) in
    let unified_typemap = create_unified_typemap iospec.typemap apispec.typemap in
	(* The synthesizer NEEDS to result in a function definition
	of that type, and a way to convert that function into
	a string for each of the backends (or into GIR).
	Right now, only C++ is a valid
	backend, so that's all that's required.  *)
    let () = if options.debug_post_synthesis then
        Printf.printf "Starting post-synthesis\n"
    else () in
	match options.post_synthesizer with
	| NoSynthesizer -> None
	| FFTSynth -> fft_synth options classmap unified_typemap names program io_pairs