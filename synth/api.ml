(* This file contains functions representing the accelerators.  
   It isn't strictly needed, as we could use the underlying
   accelerators instead, but it enables cross compilation to
   have the behaviour implemented here.  *)
open Core_kernel;;
open Spec_definition;;

type api = {
    livein: string list;
    liveout: string list;
    typemap: (string, synth_type) Hashtbl.t;
    exec: (string, synth_value) Hashtbl.t -> (string, synth_value) Hashtbl.t
}

let fft_accel_api = {
    (* In-place FFT. *)
    livein = ["fftvalues"; "length"];
    liveout = ["fftvalues"];
    typemap = (
        let tbl = Hashtbl.create (module String) in
        ignore(Hashtbl.add tbl ~key:"fftvalues" ~data:(Array(Float32)));
        ignore(Hashtbl.add tbl ~key:"length" ~data:Int32);
        tbl
    );
    (* TODO -- Write a real implementation.  *)
    exec =
        (fun vlookup ->
            vlookup)
}
