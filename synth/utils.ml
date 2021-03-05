open Core_kernel;;

let id x = x

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

let hash_table_from_list s l =
    let tbl = Hashtbl.create s in
    let () = ignore(List.map l (fun (i, t) ->
        Hashtbl.add tbl i t
    )) in
    tbl

let rec truncate_zip l1 l2 =
	match l1, l2 with
	| [], _ -> []
	| _, [] -> []
	| x :: xs, y :: ys ->
			(x, y) :: (truncate_zip xs ys)

let prepend_all x xs =
    List.map xs (fun l -> x :: l)

let max_of_int_list xs =
	List.fold xs ~f:(fun x -> fun y -> if x > y then x else y) ~init:(-1000000000)
	(* Crappy hack to regret: *)

let int_range low high =
    let rec int_range_internal low high =
        if low = high then
            []
        else
            low :: (int_range_internal (low + 1) high)
    in
    if low > high then
        []
    else
        int_range_internal low high

(* Map through f while f does not return None.  *)
let rec map_while ls f =
	match ls with
	| [] -> []
	| l :: ls ->
			match f(l) with
			| item, true -> item :: (map_while ls f)
			| item, false -> item :: []

let string_equal x y =
	(String.compare x y) = 0
