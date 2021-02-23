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


let uniq_cons eql x xs = if List.mem xs x eql then xs else x :: xs

let remove_duplicates eql xs = List.fold_right xs ~f:(uniq_cons eql) ~init:[]

let set_difference eql l1 l2 = List.filter l1 (fun x -> not (List.mem l2 x eql))
