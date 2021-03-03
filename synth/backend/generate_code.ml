open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Gir_utils;;
open Options;;
open Utils;;
open Gir;;

exception CXXGenerationException of string

let cxx_generate_imports filenames =
	(String.concat ~sep:"\n" (List.map filenames (fun name ->
		"#include \"" ^ name ^ "\""))) ^ "\n\n"

let cxx_gir_name_to_string nm =
	match nm with
	| Name(n) -> n

let output_variable_count = ref 0
let generate_out_tmp () =
    let () = output_variable_count := !output_variable_count + 1 in
    "output_temp_" ^ (string_of_int !output_variable_count)
let generate_ivar_tmp () =
    let () = output_variable_count := !output_variable_count + 1 in
    "i" ^ (string_of_int !output_variable_count)


(* Type signatures use pointer formatting.  *)
let rec cxx_type_signature_synth_type_to_string typ =
    match typ with
    | Int16 -> "short"
    | Int32 -> "int"
    | Int64 -> "long int"
    | Float16 -> "float16?(unsupported)"
    | Float32 -> "float"
    | Float64 -> "double"
    | Array(stype, _) -> (cxx_type_signature_synth_type_to_string stype) ^ " *"
    | Unit -> "void"
    (* Assume not passed as pointer.   May need to change. *)
    | Struct(sname) -> sname
    | Fun(from, tof) -> raise (CXXGenerationException "Lambdas Unsupported in C++")

(*  Type signatirue s using vector formatting.  *)
let rec cxx_vectors_type_signature_synth_type_to_string typ =
    match typ with
    | Int16 -> "short"
    | Int32 -> "int"
    | Int64 -> "long int"
    | Float16 -> "float16?(unsupported)"
    | Float32 -> "float"
    | Float64 -> "double"
    | Array(stype, _) -> "std::vector<" ^ (cxx_vectors_type_signature_synth_type_to_string stype) ^ ">"
    | Unit -> "void"
    (* Again, assume not as pointer *)
    | Struct(sname) -> sname
    | Fun(from, tof) -> raise (CXXGenerationException "Lambdas unsupported in C++")

(* match a dimtype to the /highest level name only/ *)
(* e.g. H(..., v) -> v *)
let cxx_dimtype_to_name dimtype =
    match dimtype with
    | Dimension(x :: []) ->
            (name_reference_to_string x)
            (* Think this can be achieved in the gen_json call.
               Just return a TODO note, since that's what
               has to happen.  If it's (incorrectly)
               called in the synthesis pipeline, then it'll
               error later anyway.  *)
	| EmptyDimension -> "TOFILL"
    | _ -> let () = Printf.printf "Ref is %s\n" (dimension_type_to_string dimtype) in
	raise (CXXGenerationException "Pretty sure this can't happen?")

let rec cxx_dimtype_to_definition dimtype =
    match dimtype with
            | Dimension(x :: []) -> "[" ^ (name_reference_to_string x) ^ "]"
            | _ -> raise (CXXGenerationException "Expected individual array types to be selected by the generate pass")

let rec cxx_definition_synth_type_to_string_prefix_postfix dimmap_lookup typ name =
    match typ with
    | Array(stype, _) ->
			(* This isn;t going to work for multi-dimensional arrays, but I suppose
			that s OK.  The C memory model doesn't strictly support those
			anyway, so we may be able to get away without this here.  *)
			let dimtype = Hashtbl.find_exn dimmap_lookup name in
            let postfix = cxx_dimtype_to_definition dimtype in
            let prefix, sub_postfix = cxx_definition_synth_type_to_string_prefix_postfix dimmap_lookup stype name in
            prefix, postfix ^ sub_postfix
    | othertyp ->
            (* If it's another type, then use the simple type generator *)
            (cxx_type_signature_synth_type_to_string othertyp, "")


(* definitions use array formatting so that arrays
   can be allocated on the stack.  *)
let rec cxx_definition_synth_type_to_string dimmap_lookup typ name =
    let (prefix, postfix) = cxx_definition_synth_type_to_string_prefix_postfix dimmap_lookup typ name in
    (* Prefix is like the type name, 'name' is the variable name,
       postfix is array markings like [n], and then we need
       to add a semi colon. *)
    prefix ^ " " ^ name ^ postfix ^ ";"

let cxx_names_to_type_definition (typemap: (string, synth_type) Hashtbl.t) names =
    List.map names (fun name -> (cxx_type_signature_synth_type_to_string (Hashtbl.find_exn typemap name)) ^ " " ^ name)

let rec cxx_generate_from_gir (typemap: (string, synth_type) Hashtbl.t) (binds: (string, dimension_type) Hashtbl.t) gir =
    match gir with
    | Definition(nref) ->
            let defntype = (Hashtbl.find_exn typemap (cxx_gir_name_to_string nref)) in
            cxx_definition_synth_type_to_string binds defntype (cxx_gir_name_to_string nref)
    | Sequence(girlist) ->
            String.concat ~sep:";\n\t" (List.map girlist (cxx_generate_from_gir typemap binds))
    | Assignment(fromv, tov) ->
			(cxx_generate_from_lvalue fromv) ^ " = " ^ (cxx_generate_from_rvalue tov) ^ ";"
    | LoopOver(gir, indvariable, loopmax) ->
            let indvar_name = (cxx_gir_name_to_string indvariable) in
            let loopmax_name = (cxx_generate_from_variable_reference loopmax) in
            "for (int " ^ indvar_name ^ " = 0; " ^ indvar_name ^ " < " ^ loopmax_name ^ "; " ^ indvar_name ^ "++) {\n\t\t" ^
            (cxx_generate_from_gir typemap binds gir) ^
            "\n\t}"
    | Expression(expression) ->
            (cxx_generate_from_expression expression)
    | EmptyGIR -> ";"

and cxx_generate_from_lvalue lvalue =
    match lvalue with
    | LVariable(nref) -> (cxx_generate_from_variable_reference nref)

and cxx_generate_from_variable_reference vref =
	match vref with
	| Variable(nm) ->
			cxx_gir_name_to_string nm
	| MemberReference(structref, member) ->
			(* TODO -- need to support pointer-ref
			   generation.  *)
			(cxx_generate_from_variable_reference structref) ^ "." ^ (cxx_gir_name_to_string member)
	| IndexReference(arr, ind) ->
			(cxx_generate_from_variable_reference arr) ^ "[" ^ (cxx_generate_from_expression ind) ^ "]"

and cxx_generate_from_rvalue rvalue =
    match rvalue with
    | Expression(expr) -> (cxx_generate_from_expression expr)
and cxx_generate_from_expression expr =
    match expr with
    | VariableReference(nref) -> (cxx_generate_from_variable_reference nref)
    | FunctionCall(fref, vlist) ->
            (cxx_generate_from_function_ref fref) ^ "(" ^ (cxx_generate_from_vlist vlist) ^ ");"
and cxx_generate_from_function_ref fref =
    match fref with
    | FunctionRef(nref) -> (cxx_gir_name_to_string nref)
and cxx_generate_from_vlist vlist =
    match vlist with
    | VariableList(nrefs) ->
        String.concat ~sep:", " (List.map nrefs cxx_generate_from_variable_reference)

(* Some variables are 'dead in', i.e. they don't need to be assigned
to, just allocated.  This does just the allocation part :) *)
let rec generate_empty_assign_to assname typ =
    match typ with
    (* May have to have a special case for some things here?  not sure,
    C is actually pretty good with defines-in-place. *)
    | _ ->
			let type_sig = cxx_type_signature_synth_type_to_string typ in
            type_sig ^ " " ^ assname ^ ";"

let rec generate_assign_to classmap assname fieldname typ json_ref =
	let json_ref =
		match fieldname with
		(* Some refs, e.g. after arrays are actually impllicit.  *)
		| None -> json_ref
		| Some(n) -> json_ref ^ "[\"" ^ n ^ "\"]"
	in
	match typ with
	| Array(artyp, _) ->
            let artypstr = cxx_type_signature_synth_type_to_string artyp in
			let vecname = assname ^ "_vec" in
			let resdef = "std::vector<" ^ artypstr ^ "> " ^ vecname ^ ";" in
			let loop_header = "for (auto& elem : " ^ json_ref ^ ") {" in
			let recursed_code = generate_assign_to classmap (assname ^ "_inner") None artyp "elem" in
            let in_loop_assign =
                vecname ^ ".push_back(" ^ assname ^ "_inner);" in
			let end_loop = "}" in
			(* Assign back to a pointer, since that's what we are treating arrays
			as --- may have to put some thought into indexed classes too.  Not
			100% how to go about that right now.  *)
			let post_loop = artypstr ^ " *" ^ assname ^ " = &" ^ vecname ^ "[0];" in
			resdef ^ "\n" ^ loop_header ^ "\n" ^ recursed_code ^ "\n" ^ in_loop_assign ^ "\n" ^ end_loop ^
			"\n" ^ post_loop
	| Struct(sname) ->
			(* Get the members we need to fill, and
			   then get the values.  *)
			let structmeta = Hashtbl.find_exn classmap sname in
			let struct_typemap = get_class_typemap structmeta in
			let members = get_class_fields structmeta in
			(* Pair those members with their types *)
			let memtypes = List.map members (fun mem ->
				mem, (Hashtbl.find_exn struct_typemap mem)) in
			(* Recurse to get the values *)
			let members_assigns =
				List.map memtypes (fun (mem, memtyp) -> generate_assign_to classmap (assname ^ mem) (Some(mem)) memtyp json_ref) in
			let memass_names = List.map members (fun m -> (assname ^ m)) in
			(* Now, build the struct *)
			let class_assign = if is_class structmeta then
				(* is a class *)
				(* We assume that the class has a 'simple' constructor *)
				sname ^ " " ^ assname ^ "(" ^ (String.concat ~sep:", " memass_names) ^ ");"
			else 
				(* is a struct *)
				sname ^ " " ^ assname ^ " = { " ^ (String.concat ~sep:", " memass_names) ^ "};"
			in
			String.concat ~sep:"\n" (members_assigns @ [
				class_assign
			])
	| _ ->
			let type_sig = cxx_type_signature_synth_type_to_string typ in
			type_sig ^ " " ^ assname ^ " = " ^ json_ref ^ ";"

(* Given the Input typespec, generate some code that reads
those IO values from a JSON file and puts them into values.
Returns boht the code, and a list of function args to apply.
*)
let rec generate_input_assigns classmap lenvar_bindings inps livein typemap json_ref =
	let asses = List.map livein (fun inp ->
		(* THis hsould be easy -- if it's not an array, then
			just load the value -- if it is an array, then
			do the complicated ararys stuff and recurse. *)
		let typ = Hashtbl.find_exn typemap inp in
		generate_assign_to classmap inp (Some(inp)) typ json_ref
	) in
	(* There are some variables that are 'dead' in, i.e. the
	need to be allocated (e.g. output arrays), but they don't
	need to be filled.  Call those 'deadin' values. *)
	let deadin = set_difference (fun x -> fun y -> (String.compare x y) = 0) inps livein in
    let deadin_defs = List.map deadin (fun inp ->
        let typ = Hashtbl.find_exn typemap inp in
		cxx_definition_synth_type_to_string lenvar_bindings typ inp
    ) in
	(* Hope and pray we don't end up needing to topo sort
	this shit. *)
	let all_asses = asses @ deadin_defs in
	((String.concat ~sep:"\n" all_asses), String.concat ~sep:", " inps)

(* Given a list of the liveout vars, produce an asssignment
that produces the output JSON.*)
let rec generate_output_assigns options classmap lenvar_assigns types outvars outprefix outjson =
	let asses = List.map (List.zip_exn outvars types) (fun (out, typ) ->
		let defcode, vname = (generate_output_assign options classmap lenvar_assigns typ out outprefix) in
		(* Defcode is the code to define any intermediate values needed. *)
		defcode ^ "\n" ^
		outjson ^ "[\"" ^ out ^ "\"] = " ^ vname ^ ";"
	) in
	(String.concat ~sep:"\n" asses)

(* note that this is a slightly confusing definition, because we are
trying to input from the 'out' variable into the json, not assign
to it.  *)
and generate_output_assign options classmap lenvar_assigns typ out out_prefix =
	let () = if options.debug_generate_code then
		Printf.printf "Generating outupt for %s\n" (out)
	else () in
	match typ with
	| Array(artyp, _) ->
		(* Needs to go back to a std::vector-based type *)
		let artypname = cxx_vectors_type_signature_synth_type_to_string artyp in
		let outtmp = generate_out_tmp () in
        let ivar = generate_ivar_tmp() in
		let adim = Hashtbl.find_exn lenvar_assigns out in
		let length = cxx_dimtype_to_name adim in
		let vecres = "std::vector<json> " ^ outtmp ^ ";" in
		(* TODO--- need to actually properly handle multi-dimensions here.  *)
		(* (Array indexing like this won't work for the C view of the world --probaly
		need multipied indexes or some shit. ) *)
		let assloop_header = "for (unsigned int " ^ ivar ^ " = 0; " ^ ivar ^ " < " ^ length ^ "; " ^ ivar ^ "++) {" in
		let newout = generate_out_tmp() in
		let newout_assign = artypname ^ " " ^ newout ^ " = " ^ out ^ "[" ^ ivar ^ "];" in
		let assbody, assresvar = generate_output_assign options classmap lenvar_assigns artyp newout out_prefix in
		(* Add to the array. *)
		let inloopassign = outtmp ^ ".push_back(" ^ assresvar ^ ");" in
		let loop_end = "}" in
		(String.concat ~sep:"\n" [
			vecres; assloop_header; newout_assign; assbody; inloopassign; loop_end
		], outtmp)
	| Struct(n) ->
		let json_tmp = generate_out_tmp () in
		let defn = "json " ^ json_tmp ^ ";" in
		let structdefn = Hashtbl.find_exn classmap n in
		let sub_assigns = List.map (get_class_fields structdefn) (fun a -> a) in
		(* Need the unprefixed assigns so we can get their types from the typemap.  *)
		let unprefixed_assigns = get_class_fields structdefn in
		let sub_typemap = get_class_typemap structdefn in
		let sub_types = List.map unprefixed_assigns (fun ass -> Hashtbl.find_exn sub_typemap ass) in
		let asscode = generate_output_assigns options classmap lenvar_assigns sub_types sub_assigns (out ^ ".") json_tmp in
		String.concat ~sep:"\n" [defn; asscode], json_tmp
	| _ ->
		(* We can literally just put the variable name.  *)
		"", out_prefix ^ out

(* Imports needed for the running infrastructure.  *)
let otherimports = String.concat ~sep:"\n" [
    "#include<vector>"; "#include<nlohmann/json.hpp>";
    "#include<fstream>"; "#include<iomanip>";
    "using json = nlohmann::json;" (* Not strictly an include I suppose.  *)
]

(* Given the IOSpec, generate the main function required to actually run this
thing --- it should respect the specification for taking in
as args the input JSON file, and putting the outputs of the function
in the output JSON file.  *)
let cxx_main_function options classmap (iospec: iospec) lenvar_bindings =
	let json_var_name = "input_json" in
	let header = "int main(int argc, char **argv) {" in
	let argreader = "    char *inpname = argv[1]; " in
	let resdump =   "    char *outname = argv[2]; " in
	let load_file = "    std::ifstream ifs(inpname); " in
	let load_json = "    json " ^ json_var_name ^ " = json::parse(ifs);" in
	let parse_args, argnames = generate_input_assigns classmap lenvar_bindings iospec.funargs iospec.livein iospec.typemap json_var_name in
	(* TODO -- need to handle non-void call_funcs here.  *)
	let call_func = iospec.funname ^ "(" ^ argnames ^ ");" in
	let json_out_name = "output_json" in
	let write_json_def = "    json " ^ json_out_name ^ ";" in
	let liveouttypes = List.map iospec.liveout (fun i -> Hashtbl.find_exn iospec.typemap i) in
	let gen_results = generate_output_assigns options classmap lenvar_bindings liveouttypes iospec.liveout "" json_out_name in
	let ofstream_create = "std::ofstream out_str(outname); " in
	let ofstream_write = "out_str << std::setw(4) << " ^ json_out_name ^ " << std::endl;" in
	let tail = "}" in
	String.concat ~sep:"\n" [header; argreader; resdump; load_file; load_json; parse_args;
	call_func; write_json_def; gen_results; ofstream_create; ofstream_write;
	tail]

let generate_cxx (options: options) classmap (apispec: apispec) (iospec: iospec) program =
    (* C++ only allows for single return values.  *)
    (* This could be ammened to auto-add a struct,
    but can't imagine we'd need that.  *)
    let (function_type, outv) =
        match program.returnvar with
        | None -> "void", ""
        | Some(x) -> cxx_type_signature_synth_type_to_string (Hashtbl.find_exn program.typemap x), x
    in
    (* Generate the function header *)
    let function_header =
        function_type ^ " " ^ iospec.funname ^ "(" ^
        (String.concat ~sep:"," (cxx_names_to_type_definition program.typemap program.in_variables)) ^
        ") {" 
    in
    (* Generate the actual program.  *)
    let program_string = cxx_generate_from_gir program.typemap program.lenvar_bindings program.gir in
	let ioimports = cxx_generate_imports iospec.required_includes in
    let apiimports = cxx_generate_imports apispec.required_includes in
    (* And generate the return statement *)
    let function_return =
        "return " ^ outv ^ "; }" in
	let main_func = cxx_main_function options classmap iospec program.lenvar_bindings in
    (* Generate the whole program.  *)
	String.concat ~sep:"\n" [ioimports; apiimports; otherimports; function_header; program_string; function_return; main_func]
    (* TODO --- need to include a bunch of unchanging crap, e.g. 
    arg parsing.   I expect that to change /a little/ with
    the argtypes but not much.  *)

let generate_code (options: options) classmap apispec (iospec: iospec) (programs: program list) =
	let codes = match options.target with
	| CXX -> List.map programs (generate_cxx options classmap apispec iospec)
	in
	let () =
		if options.dump_generate_program then
            Printf.printf "Generated codes are %s\n" (String.concat ~sep:"\nNEWPROGRAM\n\n" codes)
        else () in
    codes
