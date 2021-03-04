open Executable_test;;
open Cmdliner;;

let main f1 f2 =
    let result = compare_outputs f1 f2 in
    Printf.printf "Result of comparison is %b\n" (result)

let f1 =
	let doc = "JSON file 1" in
	Arg.(required & pos 0 (some string) None & info [] ~docv:"JSON1" ~doc)
let f2 =
	let doc = "JSON file 2" in
	Arg.(required & pos 1 (some string) None & info [] ~docv:"JSON2" ~doc)

let info =
	let doc = "Compare JSON files" in
	Term.info "JSONGen" ~doc

let args_t = Term.(const main $ f1 $ f2)

let () = Term.exit @@ Term.eval (args_t, info)
