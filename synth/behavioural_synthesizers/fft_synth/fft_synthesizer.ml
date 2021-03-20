open Core_kernel;;
open Fft_synthesizer_definition;;
open Fft_synthesizer_gen;;
open Spec_definition;;
open Spec_utils;;
open Program;;
open Options;;

exception FFTSynth of string

(* This is a behavioural synthesizer designed to help 'fit' FFT
functions to user code.  It's designed to do common things that
FFT functions forget/choose different behaviour for, like synthesizing
normalization/denormalization and bit-reversal.  *)
(* Because it has such a limited scope, it doesn't have
to have a particularly sane/scalable design --- I expect that
similar synth tools will be useful for the general problem of
accelerator utilization.  The key, here (as everywhere) is to
avoid overfitting a particular problem with compositionality ---
it has to be obvious to compiler developers what additional tools
the synthesizer needs to address the problem correctly. *)

(* This 100% needs some heuristic filters --- the blowup is big
and it is generating a huge number of programs when really
there are only a few sensible choices.  *)
(* IMO such an heuristic filter should be pretty easy. *)

(* Here, compositoinality is in the form of functions. *)
(* We also provide a number of sketches, designed
   for the various situations we are likely to encounter
   in FFT situations.  *)

(* This synthesizer is a bit crap at time because it doesn't exactly
capture the semantics of the underlying C.  Expect that to be
an issue eventually.  *)
(* let fs_sketches = *)
(*     [FSSeq( *)
(*         [ *)
(*         FSConditional(FSArrayOp(FSNormalize, FSArrayVariableHole), *)
(*         FSGreaterThan(FSIntVariableHole, FSConstant(Int64V(0)))); *)
(*         FSConditional(FSArrayOp(FSNormalize, FSArrayVariableHole), *)
(*         FSGreaterThan(FSIntVariableHole, FSConstant(Int64V(0)))); *)
(*         ] *)
(*     )] *)
let fs_sketches = 
    let uncond_array_op = 
        (* Unconditional operation on an array. *)
        FSArrayOp(FSArrayOpHole, FSArrayVariableHole) in
    let cond_array_op =
        FSConditional(FSArrayOp(FSArrayOpHole, FSArrayVariableHole), FSConditionalHole) in
    [
    (* Empty program --- tbh not really sure we need this here,
    but it makes testing a bit easier for now. *)
    FSSeq([]);
    (* Conditional operation on an array. *)
    cond_array_op;
    uncond_array_op;
    (* Some functions return as pairs of arrays
       rather than single arryas, so also
       look at pairs of functions.  *)
    FSConditional(FSSeq([
        uncond_array_op;
        uncond_array_op
    ]), FSConditionalHole);
    (* Unconditional also.  *)
    (* TODO -- could make this more efficient
    with some joint synthesis techniques to tie
    the two array ops together.  *)
    FSSeq([
        uncond_array_op;
        uncond_array_op;
    ])
    (* TODO --- More as required?  Perhaps that is general enough? *)
]

let fs_conditional_sketches = [
    FSGreaterThan(FSIntVariableHole, FSIntConstantHole);
    FSLessThan(FSIntVariableHole, FSIntConstantHole);
    FSGreaterThan(FSFloatVariableHole, FSFloatConstantHole);
    FSLessThan(FSFloatVariableHole, FSFloatConstantHole);
    (* PowerOfTwo(VariableHole)*)
]

let fs_array_operator = [
    FSBitReversal;
    FSNormalize;
    FSDenormalize;
]

(* Typical ways that FFT/IFFT are specified. *)
let fs_int_constants = [
    Int64V(-1); Int64V(0); Int64V(1)
]

let fs_float_constants = [
    Float64V(-1.0); Float64V(0.0); Float64V(1.0)
]

let rec to_string_structure structure =
    match structure with
    | FSConditional(act, condition) ->
            "Cond(" ^ (to_string_cond condition) ^ ": " ^ (to_string_structure act) ^ ")"
    | FSArrayOp(oper, var) ->
            "ArrayOp(" ^ (to_string_op oper) ^ ": " ^ (to_string_var var) ^ ")"
    | FSSeq(elems) ->
            String.concat ~sep:";" (List.map elems to_string_structure)
    | FSStructureHole -> "StructureHole"
and to_string_cond cond =
    match cond with
    | FSGreaterThan(v1, v2) ->
            (to_string_var v1) ^ " > " ^ (to_string_var v2)
    | FSLessThan(v1, v2) ->
            (to_string_var v1) ^ " < " ^ (to_string_var v2)
    | FSPowerOfTwo(v) ->
            "PowerOfTwo(" ^ (to_string_var v) ^ ")"
    | FSConditionalHole ->
            "ConditionalHole"
and to_string_var var =
    match var with
    | FSVariable(v) -> (name_reference_to_string v)
    | FSConstant(v) -> (synth_value_to_string v)
    | FSScalarVariableHole -> "ScalarHole"
    | FSArrayVariableHole -> "ArrayHole"
    | FSIntVariableHole -> "IntHole"
    | FSFloatVariableHole -> "FloatHole"
    | FSIntConstantHole -> "IntConstHole"
    | FSFloatConstantHole -> "FloatConstHole"
and to_string_op op = match op with
    | FSBitReversal -> "BitReversal"
    | FSNormalize -> "Normalize"
    | FSDenormalize -> "Denormalize"
    | FSArrayOpHole -> "ArrayOperatorHole"

let rec fs_fill_holes filler structure =
    match structure with
    | FSConditional(act, condition) ->
            let act_options = (fs_fill_holes filler act) in
            let cond_options = (fill_conditional_holes filler condition) in
            List.map (List.cartesian_product act_options cond_options) (fun (a, c) ->
                FSConditional(a, c)
            )
    | FSArrayOp(oper, var) ->
            let oper_options = fill_array_op_hole filler oper in
            let var_options = fill_variable filler var in
            List.map (List.cartesian_product oper_options var_options) (fun (o, v) ->
                FSArrayOp(o, v)
            )
    | FSSeq(elems) ->
            let elem_opts = List.map elems (fs_fill_holes filler) in
            List.map (Utils.cross_product elem_opts)
            (fun opt -> FSSeq(opt))
    | FSStructureHole -> raise (FFTSynth "DOn't support filling structural holes")

and fill_conditional_holes filler structure =
    match structure with
    | FSGreaterThan(v, const) ->
            let v_opts = fill_variable filler v in
            let c_opts = fill_variable filler const in
            List.map (List.cartesian_product v_opts c_opts) (fun (v, c) ->
                FSGreaterThan(v, c)
            )
    | FSLessThan(v, const) ->
            let v_opts = fill_variable filler v in
            let c_opts = fill_variable filler const in
            List.map (List.cartesian_product v_opts c_opts) (fun (v, c) ->
                FSLessThan(v, c)
            )
    | FSPowerOfTwo(v) ->
            let v_opts = fill_variable filler v in
            List.map v_opts (fun v ->
                FSPowerOfTwo(v)
            )
    | FSConditionalHole ->
            List.concat (List.map fs_conditional_sketches (fill_conditional_holes filler))
and fill_array_op_hole filler v =
    match v with
    | FSArrayOpHole ->
            List.concat (List.map fs_array_operator (fill_array_op_hole filler))
    | other -> [other]

and fill_variable filler v = 
    filler v

let hole_options options array_variables int_variables float_variables variable_type =
    let result = match variable_type with
    | FSIntConstantHole -> List.map fs_int_constants (fun x -> FSConstant(x))
    | FSFloatConstantHole -> List.map fs_float_constants (fun x -> FSConstant(x))
    | FSArrayVariableHole -> List.map array_variables (fun x -> FSVariable(x))
    | FSIntVariableHole -> List.map int_variables (fun x -> FSVariable(x))
    | FSFloatVariableHole -> List.map float_variables (fun x -> FSVariable(x))
    | FSScalarVariableHole -> List.map (int_variables @ float_variables) (fun x -> FSVariable(x))
    | FSVariable(x) -> [variable_type]
    | FSConstant(x) -> [variable_type] in
    let () = if options.debug_fft_synthesizer then
        let () = Printf.printf "Request was for %s\n" (to_string_var variable_type) in
        let () = Printf.printf "Size of result is %d\n" (List.length result) in
        ()
    else () in
    result

let evaluate_variable v = 
    v

let compare_fs (a: synth_value) b = match (int_from_value a, int_from_value b) with
    | Some(a), Some(b) -> Int.compare a b
    | _, _ -> (* At least one is none, try a float compare instead.  *)
            match (float_from_value a, float_from_value b) with
            | Some(a), Some(b) -> Float.compare a b
            (* No other cases are currently handled.  *)
            | _, _ -> raise (FFTSynth ("Type error, can't compare " ^ (synth_value_to_string a) ^ " and " ^ (synth_value_to_string b)))

let normalize arr =
    let length = float_of_int (List.length arr) in
    List.map arr (fun el -> el /. length)

let denormalize arr =
    let length = float_of_int (List.length arr) in
    List.map arr (fun el -> el *. length)

let bit_reverse n maxbits =
    (* not sure (a) what this should do for non power of two
       or (b) if it's right for non power of two.  *)
    (* Also should be clear that I'm not 100% sure it matches
    the C implmentatin we use --- potential source of
    hard to find bugs IMO.  *)
    let result = ref 0 in
    let nref = ref n in
    let xref = ref maxbits in
    (* let () = Printf.printf "N is %d\n" (!nref) in *)
    while (!xref) <> 1 do
        (* let () = Printf.printf "XRef %d" (!xref) in *)
        result := (!result) lsl 1;
        result := (!result) lor ((!nref) land 1);
        nref := (!nref) lsr 1;
        (* let () = Printf.printf "Result so far is %d\n" (!result) in *)

        xref := (!xref) lsr 1;
    done;
    !result

let bit_reversal arr =
    let array_version = Array.of_list arr in
    let array_length = Array.length array_version in
	let () = for i = 0 to array_length - 1 do
        let reversed = bit_reverse i array_length in
        if reversed < i then
            let tmp = Array.get array_version i in
            let () = Array.set array_version i (Array.get array_version reversed) in
            let () = Array.set array_version reversed tmp in
            ()
        else
            ()
    done in
    Array.to_list array_version

let rec runner program (inputs: (string, synth_value) Hashtbl.t) =
    match program with
    | FSConditional(act, condition) -> (
            match eval_condition condition inputs with
            | true -> runner act inputs
            | false -> ()
    )
    | FSSeq(elems) ->
            (* These things mutate the inputs.  *)
            ignore(List.map elems (fun e ->
                runner e inputs
            ))
    | FSArrayOp(action_name, FSVariable(on)) ->
            let arr = match array_from_value (Hashtbl.find_exn inputs (name_reference_to_string on)) with
            | Some(arr) -> arr
            | None -> raise (FFTSynth "TYpe error")
            in
            (
            match action_name with
            | FSBitReversal ->
                    let reversed = bit_reversal arr in
                    Hashtbl.set inputs (name_reference_to_string on) (ArrayV(reversed))
            | FSNormalize ->
                    let values = List.map arr (fun elt -> match float_from_value elt with
                        | Some(f) -> f
                        | None -> raise (FFTSynth "Type error")
                    )
                    in
                    (* TODO --- could do better than rewraping in float64 probably ---
                     note is tied to comparisons --- which is tied to reading
                     in values from json_utils. *)
                    let result = List.map (normalize values) (fun x -> Float64V(x)) in
                    Hashtbl.set inputs (name_reference_to_string on) (ArrayV(result))
            | FSDenormalize ->
                    let values = List.map arr (fun elt -> match float_from_value elt with
                        | Some(f) -> f
                        | None -> raise (FFTSynth "Type error")
                    ) in
                    let result = List.map (denormalize values) (fun x -> Float64V(x)) in
                    Hashtbl.set inputs (name_reference_to_string on) (ArrayV(result))
            | FSArrayOpHole ->
                    raise (FFTSynth "Can't execute algs with holes")
            )
    | FSArrayOp(action_name, _) ->
            raise (FFTSynth "Unsupported action on non-variable")
    | FSStructureHole ->
            raise (FFTSynth "Can't emulate a structural hole!")

and eval_condition cond inputs =
    match cond with
    | FSGreaterThan(vref1, vref2) ->
            let v1 = eval_variable vref1 inputs in
            let v2 = eval_variable vref2 inputs in
            (compare_fs v1 v2) = 1
    | FSLessThan(vref1, vref2) ->
            let v1 = eval_variable vref1 inputs in
            let v2 = eval_variable vref2 inputs in
            (compare_fs v1 v2) = -1
    | FSPowerOfTwo(v) ->
            (* Unimplemented right now *)
            raise (FFTSynth "Unimplemented")
    | FSConditionalHole ->
            raise (FFTSynth "Can't evaluate a hole!")

and eval_variable v (inputs: (string, synth_value) Hashtbl.t): synth_value =
	match v with
    | FSConstant(v) -> v
    | FSVariable(n) ->
        let v = Hashtbl.find_exn inputs (name_reference_to_string n) in
        v
    | _ -> raise (FFTSynth "Can't eval a hole")

(* Generate the assignment options for each class of variable.  *)
(* This is really just a shitty heuristic, and it's just crap code
too.  Want to think of a better way of doing this.  *)
let rec split_variables classmap typemap variables =
    (* Get the type of each variable.  *)
    let types = List.map variables (fun v -> Hashtbl.find_exn typemap (name_reference_to_string v)) in
    let arr_vars, i_vars, f_vars, s_vars = List.fold (List.zip_exn types variables) ~init:([], [], [], [])
        ~f:(fun (a, i, f, s) -> fun (t, v) ->
            match t with
            | Int16 -> (a, v :: i, f, s)
            | Int32 -> (a, v :: i, f, s)
            | Int64 -> (a, v :: i, f, s)
            | Float16 -> (a, i, v :: f, s)
            | Float32 -> (a, i, v :: f, s)
            | Float64 -> (a, i, v :: f, s)
            | Struct(nm) -> (a, i, f, (v, nm) :: s)
            (* TODO -- perhaps we should support higher dimensions? *)
            | Array(subty, _) -> (v :: a, i, f, s)
            | Unit -> raise (FFTSynth "Unit not supproted")
            | Fun(_, _) -> raise (FFTSynth "Higher order functions not supported")
        ) in
    let struct_name_types = List.map s_vars (fun (varname, structname) ->
        let struct_metadata = Hashtbl.find_exn classmap structname in
        let structtypemap = get_class_typemap struct_metadata in
        let structmembers = List.map (get_class_members struct_metadata) (fun mem -> Name(mem)) in
        let (sarr, si, sf) = split_variables classmap structtypemap structmembers in

        (* We need to prepend the structname to everything here.  *)
        let prepend_sname = name_reference_concat varname in
        (List.map sarr prepend_sname,
         List.map si prepend_sname,
         List.map sf prepend_sname
        )
    ) in
    (* Probably could be done in a more scalable manner.  Anyway... *)
    List.fold struct_name_types ~init:(arr_vars, i_vars, f_vars) ~f:(fun (a, i, f) ->
            fun (a2, i2, f2) ->
                (a @ a2, i @ i2, f @ f2)
    )

class fft_synth_manipulator hole_opts =
    object
        inherit [fs_structure] Generic_sketch_synth.synth_manipulator as super
        method to_string (fs: fs_structure) =
            to_string_structure fs
        method fill_holes structs =
            List.concat (
                List.map structs (fs_fill_holes hole_opts)
            )
        method runner prog state =
            runner prog state
    end

(* Now, run a generic sketch-based synthesis from these sketches. *)
let fft_synth options classmap typemap variables (gir_program: program) iopairs: post_behavioural_program option =
    let array_variables, int_variables, float_variables = split_variables classmap typemap variables in
    let hole_opts = hole_options options array_variables int_variables float_variables in
    let fft_manip = ((new fft_synth_manipulator hole_opts) :> (fs_structure Generic_sketch_synth.synth_manipulator)) in
    let prog_opts = Generic_sketch_synth.generate_options options fft_manip fs_sketches in
	let valid_programs = Generic_sketch_synth.eval options fft_manip prog_opts iopairs in
	match valid_programs with
	| [] -> None
	| x :: xs ->
			Some({
				program = (generate_program_string options gir_program.lenvar_bindings x)
			})