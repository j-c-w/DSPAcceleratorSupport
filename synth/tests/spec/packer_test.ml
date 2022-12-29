open Core;;
open Spec_definition;;
open Spec_utils;;
open Binary_packer;;
open Alcotest;;

let test_unpack_struct () =
	let tmap = Hashtbl.create (module String) in
	let cmap = Hashtbl.create (module String) in
	let class_tmap = Hashtbl.create (module String) in
	(* Create the classmaps *)
	Hashtbl.add class_tmap ~key:"f64" ~data:Float64;
	Hashtbl.add class_tmap ~key:"i32" ~data:Int32;
	Hashtbl.add class_tmap ~key:"i32_2" ~data:Int32;
	Hashtbl.add class_tmap ~key:"i64" ~data:Int64;
	Hashtbl.add class_tmap ~key:"f32" ~data:Float32;
	Hashtbl.add tmap ~key:"example" ~data:(Struct("str"));
	Hashtbl.add cmap ~key:"str" ~data:StructMetadata({
		members = ["f64"; "i32"; "i32_2"; "i64"; "f32"];
		typemap = class_tmap;
		io_typemap = class_tmap
	});
	(* Create some values. *)

	let vmap = Hashtbl.create (module String) in
	Hashtbl.add vmap ~key:"f64" ~data:(Float64V(1.0))
	Hashtbl.add vmap ~key:"i32" ~data:(Int32V(1000))
	Hashtbl.add vmap ~key:"i32_2" ~data:(Int32V(32))
	Hashtbl.add vmap ~key:"i64" ~data:(Int64V(-11))
	Hashtbl.add vmap ~key:"f32" ~data:(Float32V(-0.1))

	Alcotest.(check (bool))

let main () =
	[
		"unpacking-test",
		[
			test_case "struct_unpack" `Quick test_unpack_struct
		]
	]
