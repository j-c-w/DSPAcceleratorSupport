open Core_kernel;;

let rec cross_product ls =
	match ls with
	| [] -> []
	| [x] ->
			(* let () = Printf.printf "Length of x is %d" (List.length x) in *)
			List.map x (fun value -> [value])
	| (x :: xs) ->
			let subcross = cross_product xs in
			List.concat(
			List.map subcross (fun subprod ->
				List.map x (fun value -> 
					value :: subprod
				)
			)
			)

