open Core_kernel;;
open Generate_gir
open Alcotest;;
open Spec_definition;;

let string_list_list_to_string ls =
	String.concat ~sep:"." (List.map ls (fun l -> String.concat ~sep:"." l))

let expand_simple_struct () =
	let testtmap = {
		variable_map = Hashtbl.create (module String);
		classmap = Hashtbl.create (module String);
		alignment_map = Hashtbl.create (module String);
		original_typemap = None
	}
	in

	let typ = Array(Struct("teststr"), SingleDimension(DimConstant(10))) in
	(* Setup the typemap properly.  *)
	let struct_classmap =  {
		members = ["mem1"; "mem2"];
		typemap = Hashtbl.create (module String);
		io_typemap = Hashtbl.create (module String);
	} in
	let _ = Hashtbl.add struct_classmap.typemap "mem1" Int32 in
	let _ = Hashtbl.add struct_classmap.typemap "mem2" (Array(Float32, SingleDimension(DimConstant(10)))) in
	let _ = Hashtbl.add testtmap.classmap "teststr" (StructMetadata(struct_classmap)) in

	let expanded_types = get_copy_types_for testtmap typ ["basename1"] ["basename2"] in
	let () = Printf.printf "Size of expanded types is %d\n" (List.length expanded_types) in
	let _ = Alcotest.(check (int)) "same int" 2 (List.length expanded_types) in
	let () = Printf.printf "FQDNs is %s\n" (String.concat ~sep:", " (List.map expanded_types (fun (t, n1, n2) -> string_list_list_to_string n1))) in
	()

let main () = 
	[
		"expand-types-test",
		[
			test_case "expand_simple_struct" `Quick expand_simple_struct;
		];
	]
