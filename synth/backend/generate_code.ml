open Core;;
open Spec_definition;;
open Spec_utils;;
open Gir_utils;;
open Options;;
open Utils;;
open Gir;;
open Program;;
open Program_utils;;
open Skeleton_utils;;
open Compile_settings;;

exception CXXGenerationException of string

let cxx_generate_imports filenames =
	(String.concat ~sep:"\n" (List.map filenames ~f:(fun name ->
		if (Filename.check_suffix name ".h") || (Filename.check_suffix name ".c") then
			"extern \"C\" {\n#include \"" ^ name ^ "\"\n}\n"
		else
			"#include \"" ^ name ^ "\""))) ^ "\n\n"

let cxx_gir_name_to_string nm =
	match nm with
	| Name(n) -> n

let output_variable_count = ref 0
let generate_out_tmp () =
    let () = output_variable_count := !output_variable_count + 1 in
    "output_temp_" ^ (string_of_int !output_variable_count)
let generate_input_tmp () =
	let () = output_variable_count := !output_variable_count + 1 in
	"input_temp_" ^ (string_of_int !output_variable_count)
let generate_ivar_tmp () =
    let () = output_variable_count := !output_variable_count + 1 in
    "i" ^ (string_of_int !output_variable_count)
let generate_generic_tmp () =
	let () = output_variable_count := !output_variable_count + 1 in
	"temp_" ^ (string_of_int !output_variable_count)

let cxx_generate_skeleton_description skel = (* For debugging purposes, generate
	the skeleton from which the GIR was generated.  *)
	"/* Orignal skeleton is: \n" ^
	(skeleton_pairs_to_string skel) ^
	"\n*/\n"

let cxx_generate_typemap_description tmap =
	(* Also for debugging, generate the typemap, with infered
	info on it. *)
	"/* Typemap is :\n " ^
	(typemap_to_string tmap) ^ "\n*/\n"

let accelerator_timer_functions = "

clock_t AcceleratorStart;
clock_t AcceleratorTotalNanos = 0;
void StartAcceleratorTimer() {
	AcceleratorStart = clock();
}

void StopAcceleratorTimer() {
	AcceleratorTotalNanos +=
		(clock()) - AcceleratorStart;
}
"

(* Type signatures use pointer formatting.  *)
let rec cxx_type_signature_synth_type_to_string typ =
    match typ with
	| Bool -> "bool"
	| Int8 -> "char"
    | Int16 -> "short"
    | Int32 -> "int"
    | Int64 -> "long int"
	| UInt8 -> "unsigned char"
	| UInt16 -> "unsigned short"
	| UInt32 -> "unsigned int"
	| UInt64 -> "unsigned long int"
    | Float16 -> "float16?(unsupported)"
    | Float32 -> "float"
    | Float64 -> "double"
	| String -> "char *"
    | Array(stype, _) -> (cxx_type_signature_synth_type_to_string stype) ^ " *"
	| Pointer(stype) -> (cxx_type_signature_synth_type_to_string stype) ^ " *"
    | Unit -> "void"
    (* Assume not passed as pointer.   May need to change. *)
    | Struct(sname) -> sname
    | Fun(from, tof) -> raise (CXXGenerationException "Lambdas Unsupported in C++")

(*  Type signatirue s using vector formatting.  *)
let rec cxx_vectors_type_signature_synth_type_to_string typ =
    match typ with
	| Bool -> "bool"
	| Int8 -> "char"
    | Int16 -> "short"
    | Int32 -> "int"
    | Int64 -> "long int"
	| UInt8 -> "unsigned char"
	| UInt16 -> "unsigned short"
	| UInt32 -> "unsigned int"
	| UInt64 -> "unsigned long int"
    | Float16 -> "float16?(unsupported)"
    | Float32 -> "float"
    | Float64 -> "double"
	| String -> "char *"
	(* I think we need to use a pointer notation here, because
	it's assigned to directly from the internal value, and
	we can't cast arrays to vectors.  *)
	(* Not 100% sure if this function has any point outside
	of this, but it is called within the scope of assigning
	to vectors. *)
    | Array(stype, _) -> "" ^ (cxx_vectors_type_signature_synth_type_to_string stype) ^ "*"
	| Pointer(stype) -> (cxx_vectors_type_signature_synth_type_to_string stype) ^ "*"
    | Unit -> "void"
    (* Again, assume not as pointer *)
    | Struct(sname) -> sname
    | Fun(from, tof) -> raise (CXXGenerationException "Lambdas unsupported in C++")

let rec cxx_generate_from_synth_value typemap svalue =
	match svalue with
	| BoolV(b) -> if b then "1" else "0" (* Use C bools isntead maybe? *)
	| Int8V(v) -> string_of_int v
	| Int16V(v) -> string_of_int v
	| Int32V(v) -> string_of_int v
	| Int64V(v) -> string_of_int v
	| UInt8V(v) -> string_of_int v
	| UInt16V(v) -> string_of_int v
	| UInt32V(v) -> string_of_int v
	| UInt64V(v) -> string_of_int v
	| Float16V(f) -> string_of_float f
	| Float32V(f) -> string_of_float f
	| Float64V(f) -> string_of_float f
	| StringV(v) -> "\"" ^ v ^ "\""
	| UnitV -> "? /* (Unsupported Unit Value generation in C) */"
	| PointerV(v) ->
			"&(" ^ (cxx_generate_from_synth_value typemap v) ^ ")"
	| ArrayV(vls) ->
			"{" ^ (String.concat ~sep:", " (List.map vls ~f:(cxx_generate_from_synth_value typemap))) ^ "}"
	| StructV(nam, values) ->
			let keys = get_class_members (Hashtbl.find_exn typemap.classmap nam) in
			"{" ^
				String.concat ~sep:", \n"
				(List.map (keys) ~f:(fun key ->
					let value = Hashtbl.find_exn values key in
					"." ^ key ^ " = " ^ (cxx_generate_from_synth_value typemap value)
				)) ^
			"}"
	(* Not sure how tf to implement this --- suspect it could just
	be e.g. &(function_name)?  But not sure in C.  *)
	| FunV(vs) -> (raise (CXXGenerationException("Function pointer generation not yet supported")))

(* The aim of this is to avoid loads of empty newlines--- they
build up, partiuclarly in the precode generation.  Delete them,
but don't get ride of important newlines at the end of things :) *)
let trim x =
	match String.strip x with
	| "" -> ""
	| x -> x ^ "\n"

let rec cxx_name_reference_to_string typemap n =
	match n with
	| AnonymousName -> raise (SpecException "Unsupport anon name in backend")
	| Name(n) -> n
	| StructName([]) -> ""
	(* Assume well-formatted struct name *)
	| StructName([x]) -> cxx_name_reference_to_string typemap x
	| StructName(x :: xs) ->
			let rec get_subtype vtyp = match vtyp with
			| Pointer(vs) ->
					let _, map = get_subtype vs in
					(* Use the -> reference mode *)
					"->", map
			| Array(vs, _) -> raise (CXXGenerationException "Unsupported array generationin name_reference")
			(* Use the "." joiner mode for a struct.  *)
			| Struct(nms) -> ".", get_class_typemap (Hashtbl.find_exn typemap.classmap nms)
			| other -> "", typemap.variable_map
			in
			let xjoiner, map = get_subtype (Hashtbl.find_exn typemap.variable_map (name_reference_to_string x)) in
			let new_typemap =
				{
					typemap with variable_map = map
				}
			in
			(name_reference_to_string x) ^ xjoiner ^ (cxx_name_reference_to_string new_typemap (StructName(xs)))

let cxx_dimension_value_to_string typemap context dvalue =
	match dvalue with
	| DimConstant(i) -> (string_of_int i) (* No context to consts *)
	| DimVariable(n, DimEqualityRelation) ->
			context ^ (cxx_name_reference_to_string typemap n)
	| DimVariable(n, DimPo2Relation) ->
			"(1 << (" ^ context ^ (cxx_name_reference_to_string typemap n) ^ "))"
	| DimVariable(n, DimDivByRelation(x)) ->
			"(" ^ context ^ (cxx_name_reference_to_string typemap n) ^ " / " ^ (string_of_int x) ^ ")"

(* match a dimtype to the /highest level name only/ *)
(* e.g. H(..., v) -> v *)
let cxx_dimtype_to_name typemap context dimtype =
    match dimtype with
    | SingleDimension(x) ->
            (cxx_dimension_value_to_string typemap context x)
	| MultiDimension(xs, op) ->
			(
            match op with
            | DimMultiply ->
                    "(" ^ (String.concat (List.map xs ~f:(cxx_dimension_value_to_string typemap context)) ~sep:" * ") ^ ")"
            )
            (* Think this can be achieved in the gen_json call.
               Just return a TODO note, since that's what
               has to happen.  If it's (incorrectly)
               called in the synthesis pipeline, then it'll
               error later anyway.  *)
	| EmptyDimension -> "TOFILL"

let rec cxx_dimtype_to_definition typemap context dimtype dim_ratio_modifier =
    match dimtype with
            | SingleDimension(x) -> "[" ^ (dim_ratio_modifier (cxx_dimension_value_to_string typemap "" x)) ^ "]"
			| MultiDimension(xs, op) ->
					(
					match op with
					| DimMultiply ->
                            "[" ^ (String.concat (List.map xs ~f:(cxx_dimension_value_to_string typemap context)) ~sep: "*") ^ "]"
					)
			| EmptyDimension -> "TOFILL"

let rec cxx_definition_synth_type_to_string_prefix_postfix typemap typ name dim_ratio_modifier outermost =
    match typ with
    | Array(stype, dimtype) ->
			(* For multi-dimensional arrays, the outer-most arary
			is specified using [ ] notation, and the inner
			arrays are treated as pointers.  It's not super
			clear that this is the only way to do thing,
			e.g. you could imagine a function signature
			that looks like f(x[n][m]) for which this
			is not compatible.  (This is compatible with
			the much more common f( **x) style signature).

			IMO the best fix would re-jig the types to use
			a combination of the pointer and array
			types to mean something.  *)
			(* TODO --- ^^^ Support boxy arrays with this
		and use the pointer(array(...)) to support 
		arrays like float ***x *)
			let prefix, postfix =
				if outermost then
					"", cxx_dimtype_to_definition typemap "" dimtype dim_ratio_modifier
				else
					"*", ""
			in
            let sub_prefix, sub_postfix = cxx_definition_synth_type_to_string_prefix_postfix typemap stype name dim_ratio_modifier false in
            sub_prefix ^ prefix, postfix ^ sub_postfix
    | othertyp ->
            (* If it's another type, then use the simple type generator *)
            (cxx_type_signature_synth_type_to_string othertyp, "")

let rec cxx_escaping_definition_synth_type_to_string_prefix_postfix typemap context typ dimratio_modifier =
    match typ with
    | Array(stype, dimtype) ->
            let prefix, subsize = cxx_escaping_definition_synth_type_to_string_prefix_postfix typemap context stype dimratio_modifier in
            let dim = cxx_dimtype_to_name typemap context dimtype in
            (* THe concept is this will be expanded as:
                prefix NAME = (prefix) malloc(size);
               But, we only use malloc if this is actually
               an array.
                *)
            prefix ^ "*", subsize ^ "*" ^ dim
	| Pointer(stype) ->
			let prefix, subsize = cxx_escaping_definition_synth_type_to_string_prefix_postfix typemap context stype dimratio_modifier in
			prefix ^ "*", subsize
	| String ->
			let tyname = cxx_type_signature_synth_type_to_string String in
			(* OK -- So FACC makes the decision to treat
			strings as dimensionless (since
			they are going to be null-terminated
			and so won't have lengths represented in the available
			variables.  However, this means
			that if they have to be allocated, we don't really
			know how much space to allocate.  synthesizer.h
			provides a tunable def that specifies
			the amount of space to allocate such strings.  *)
			tyname, dimratio_modifier ("sizeof(MAX_STRING_SIZE)")
    | othertyp ->
            let tyname = cxx_type_signature_synth_type_to_string othertyp in
            tyname, dimratio_modifier ("sizeof(" ^ tyname ^ ")")

(* This is like a question of: if we are trying to return this
variable, does it have to be malloced?*)
let is_malloc_type typemap typ =
	match typ with
	| Array(stype, _) -> true
	| Pointer(_) -> true
	| String -> true
	| other -> false

let get_dimension_modifer dim_orig dim_infered modifier =
	match dim_orig with
	| SingleDimension(DimVariable(_, DimEqualityRelation)) ->
			(* Can do lots of conversions here.  *)
			(
		match dim_infered with
			| SingleDimension(DimVariable(_, DimEqualityRelation)) -> (fun v -> v ^ modifier)
			| SingleDimension(DimVariable(_, DimPo2Relation)) -> (fun v -> "(1 << " ^ v ^ ")" ^ modifier)
			| SingleDimension(DimVariable(_, DimDivByRelation(x))) ->
					(fun v ->  "(" ^ v ^ " / " ^ (string_of_int x) ^ ")" ^ modifier)
			| SingleDimension(DimConstant(_)) -> (fun v -> v ^ modifier)
			| EmptyDimension -> (fun v -> v ^ modifier)
			| MultiDimension(vs, op) ->
					(* Don't think this is valid? *)
					assert false
			)
	| other ->
			(* Otehrwise, these are going to have to be
			equal I think.  *)
			let () = assert (dimension_type_equal dim_orig dim_infered) in
			(fun v -> v ^ modifier)

(* This is basically saying: if we are allocating
space for t1, but then cast to an array of length
t2 with dimension 'n', what factor do we need to
adjust n by to allocate the right ammount of space
in terms of type t1.  *)
let compute_dimension_ratio_modifier t1 t2 =
	if synth_type_equal t1 t2 then
		(fun v -> v)
	else
		(* So this should only ever be called with unequal
			when type inference has been applied.  In that
			case, we aren't trying to handle a general case,
			but rather just a list of special cases that the
			structure inference phase can infer. 

			I suppose if the heuristic for that gets better
			this will have to get more complex, but for
			now it can just be a simple pattern match. *)
		match t1, t2 with
		| Array(Struct("facc_2xf32_t"), dim_op1), Array(Float32, dim_op2) ->
                get_dimension_modifer dim_op1 dim_op2 "/ 2"
		| Array(Struct("facc_2xf64_t"), dim_op1), Array(Float64, dim_op2) ->
                get_dimension_modifer dim_op1 dim_op2 "/ 2"
		| Array(Float32, dim_op1), Array(Struct("facc_2xf32_t"), dim_op2) ->
                get_dimension_modifer dim_op1 dim_op2 "* 2"
		| Array(Float64, dim_op1), Array(Struct("facc_2xf64_t"), dim_op2) ->
                get_dimension_modifer dim_op1 dim_op2 "* 2"
        | _, _ ->
                raise (CXXGenerationException ("Unexpected infered type pair " ^ (synth_type_to_string t1) ^ ", " ^ (synth_type_to_string t2)))

(* Constant array types are adjusted appropriately
so that we don't end up with the odd situation of
having infered an X element array when in fact
it is really a 2 * X element array.  Makes
some sense to not have these line up with dimvariables,
but not with dim constants.  *)
let use_dim_ratio_modifier adim =
	match adim with
	| SingleDimension(DimConstant(_)) -> false
	| SingleDimension(DimVariable(_)) -> true
    | MultiDimension(_, _) -> true
	| EmptyDimension -> false (* This can arise wehn doing json generation. *)

let is_heap_allocation_mode (alloc_mode: allocation_mode) =
	match alloc_mode with
	| HeapAllocationMode -> true
	| _ -> false

let assignment_for_type options typemap escapes alignment dim_ratio_modifier context name typ initial_value =
	let static_prefix = match options.compile_settings.allocation_mode with
	| StaticAllocationMode -> "static "
	| HeapAllocationMode | StackAllocationMode -> ""
	in
	let alignment_value = match alignment with
	| None -> string_of_int 0
	| Some(a) -> string_of_int a
	in
	(* If there is a static declaration of this type, need to
	expand that here.  *)
	let assign_postfix =
		match initial_value with
		| Some(v) ->
				(* Can't assign values in some cases.  *)
				let () = assert ((not escapes) && (not (is_heap_allocation_mode options.compile_settings.allocation_mode))) in
				" = " ^ (cxx_generate_from_synth_value typemap v)
		| None -> ""
	in
	let assignment =
		match options.compile_settings.allocation_mode with
				(* While sometimes it doesn't matter
				if the variable escapes if it's static, in practice, 
				if this is an escaping variable, then it sill
				needs to be malloc'ed, as that's what the user
				code will have done (so it'll be freed) *)
				(* I think that the analysis about whether
				this can be done is likely to be too complicated
				to be successful in anything but trivial
				cases, so we'll make sure to malloc anything
				that escapes here.

				The analysis would  be a dataflow analysis
				on the return value, which tracks 
				whether the array can be accessed after
				the data is overwritten.  

				In reality, we might have some embedded systems
				where the original FFT was written with
				a statically allocated array, although
				in those cases, I'd expect that to
				be presented to FACC as a liveout
				'function parameter'. *)
		| StackAllocationMode | StaticAllocationMode ->
				let align_postfix = match alignment with
						| None -> ""
						| Some(a) ->
							"__attribute__((__aligned__(" ^ (string_of_int a) ^ ")))"
				in
				if escapes then
					(* If the variable escapes, then we need to malloc it.  *)
					let usemalloc = is_malloc_type typemap typ in
					let (prefix, asize) = cxx_escaping_definition_synth_type_to_string_prefix_postfix typemap context typ dim_ratio_modifier in
					if usemalloc then
						(* Malloc'ed values are not static.  *)
						prefix ^ " " ^ name ^ " = (" ^ prefix ^ ") facc_malloc (" ^ alignment_value ^ ", " ^ asize ^ ");"
					else
						(* Unless it is a primitive *)
						static_prefix ^ " " ^ prefix ^ " " ^ name ^ align_postfix ^ assign_postfix ^ ";"
				else
					let (prefix, postfix) =
						cxx_definition_synth_type_to_string_prefix_postfix typemap typ name dim_ratio_modifier true in
					(* Prefix is like the type name, 'name' is the variable name,
					   postfix is array markings like [n], and then we need
					   to add a semi colon. *)
					static_prefix ^ prefix ^ " " ^ name ^ postfix ^ align_postfix ^ assign_postfix ^ ";"
		| HeapAllocationMode ->
				let usemalloc = is_malloc_type typemap typ in
				let (prefix, asize) = cxx_escaping_definition_synth_type_to_string_prefix_postfix typemap context typ dim_ratio_modifier in
				if usemalloc then
					(* Malloc'ed values are not static.  *)
					prefix ^ " " ^ name ^ " = (" ^ prefix ^ ") facc_malloc (" ^ alignment_value ^ ", " ^ asize ^ ");"
				else
					let align_postfix = match alignment with
					| None -> ";"
					| Some(a) ->
							"__attribute__((__aligned__(" ^ alignment_value ^ ")));"
					in
					(* Do not malloc if this is a primitive *)
					static_prefix ^ prefix ^ " " ^ name ^ align_postfix ^ assign_postfix ^ ";"
	in
	assignment

(* definitions use array formatting so that arrays
   can be allocated on the stack.  *)
(* 'Context' refers to the context that any variable might exist in.  *)
let rec cxx_definition_synth_type_to_string options typemap alignment escapes typ base_type context name initial_value =
	let () = if escapes then
		match initial_value with
		| None -> ()
		(* In C, an escaping type needs the equality to malloc
			for some types.  (e.g. a struct or array).  As a result,
			we can't use the initializer.  If that needs to happen,
			some of the stuff below, particularly the sub-initialization
			stuff, needs to be adjusted.
			*)
		| Some(x) -> raise (CXXGenerationException "Unsupported initial value on escaping type")
	else () in
	let has_initializer = match initial_value with
	| Some(_) -> true
	| None -> false
	in
	(* What ratio should be infered by the difference
	between the sizes of the respective types.  *)
	let dim_ratio_modifier =
		compute_dimension_ratio_modifier typ base_type in
	(* Allocate the space for the item.   *)
	let alloc = assignment_for_type options typemap escapes alignment dim_ratio_modifier context name typ initial_value in

	(* If the type we are defining is an array, there are several classes of subtype that
       we need to generate an assignment loop for:  other arrays, pointers, structs (sometimes, opts probably
	   good).  *)
	let generate_assign_loop_for dim sub_def sub_def_name =
		(* let () = Printf.printf "Context is %s\n" context in *)
		let dim_length = cxx_dimtype_to_name typemap context dim in
		(* let () = Printf.printf "Dim length is %s\n" dim_length in *)

		let index_variable = generate_ivar_tmp () in
		String.concat [
			alloc; "\n";
			"for (int "; index_variable; " = 0; "; index_variable; "++; "; index_variable; " < "; dim_length; ") {\n";
			sub_def; ";\n";
			name; "["; index_variable; "] = "; sub_def_name; ";\n";
			"}"
		]
	in
	if has_initializer then
		alloc (* Don't need to do sub-initializations, as those are already
			   covered by an initilializer.  *)
	else
	(
		match typ with
		| Array(sty, dim) ->
				let () =
					if options.debug_generate_malloc then
						Printf.printf "Looking at mallocing of array subtype %s\n" (synth_type_to_string sty)
					else ()
				in
				let base_type_stype =
					match base_type with
					| Array(ssty, sdim) -> ssty
					| _ -> assert false (* Think this isn't possible right now? *)
				in
				(* Go through an initialize each elt of the array if requierd.  *)
				(
				match sty with
				(* These two need allocations, everything else does not.  *)
				| Array(ssty, sdim) ->
						let sub_def_name = name ^ "_sub_element" in
						(* Alignment doesn't pass onto sub elts? Do we need a decision here eventually? *)
						(* We use None for the initial value because this is a sub-initalization --- As far as I know,
						there's no sane reason we'd want a partial initialization of a type --- either we'd want
						it all (top level) or none? *)
						let sub_def = cxx_definition_synth_type_to_string options typemap None escapes sty base_type_stype context sub_def_name None in

						generate_assign_loop_for dim sub_def sub_def_name
				| Pointer(pointed) ->
						let sub_def_name = name ^ "_sub_element" in
						let sub_def = cxx_definition_synth_type_to_string options typemap None escapes sty base_type_stype context sub_def_name None in

						generate_assign_loop_for dim sub_def sub_def_name
				| String ->
						let sub_def_name = name ^ "_sub_element" in
						let sub_def = cxx_definition_synth_type_to_string options typemap None escapes sty base_type_stype context sub_def_name None in

						generate_assign_loop_for dim sub_def sub_def_name
				| Struct(structname) ->
						(* Really, this doesn't have to happen like this --- we need a heuristic that tells
						us whether the struct has to be initialized ---- expect that this might be relevant. *)
						let sub_def_name = name ^ "_sub_element" in
						let sub_def = cxx_definition_synth_type_to_string options typemap None escapes sty base_type_stype context sub_def_name None in

						generate_assign_loop_for dim sub_def sub_def_name
				| other ->
						(* These are arrays of primitive types, which means that they don't actually need complex
						assignment types.  *)
						alloc
				)
				(* endif *)
		| String ->
				let () = if options.debug_generate_malloc then
					Printf.printf "Generating malloc call for string. \n" else () in
				alloc
		| Struct(s) ->
				let () = if options.debug_generate_malloc then
					Printf.printf "generating malloc call for strct with name %s\n" s
				else ()
				in
				(* Depending on the elements of the struct, we may or may not need to go through it.  *)
				let meta = Hashtbl.find_exn typemap.classmap s in
				let members = get_class_members meta in
				let class_typemap = get_class_typemap meta in

				(* Get the type of each member, and if we have to alloc, then do that.  *)
				let members = sort_members_by_type { typemap with variable_map = class_typemap } members in
                let struct_assignments = List.map members ~f:(fun member ->
					let () = if options.debug_generate_malloc then Printf.printf "Doing sub-struct assignment for member %s\n" (member) else () in
					let member_type = Hashtbl.find_exn class_typemap member in
					if is_malloc_type typemap member_type then
						(* Do the sub-malloc: *)
						let sub_def_name = name ^ "_" ^ member ^ "_alloc" in

						(* Note that we use the same type for the member type and the base_type for that member --- not
						100% sure this is correct, but I can't otherwise remember where I put it... *)
						let sub_typemap = { typemap with variable_map = class_typemap } in
						let subdef = cxx_definition_synth_type_to_string options sub_typemap None escapes member_type member_type (context ^ name ^ ".") sub_def_name None in
						let assign =
							name ^ "." ^ member ^ " = " ^ sub_def_name ^ ";"
						in
						subdef ^ "\n" ^ assign
					else
						""
				) in

				String.concat ~sep:"\n" ([
					alloc
				] @ struct_assignments)
		| Pointer(p) ->
				(* Note that this computes tthe sub-malloc for the subtype regardless of the sub-type mallocable
				property.  *)
				let pointer_is_malloc = is_malloc_type typemap p in
				let () = if options.debug_generate_malloc then
					Printf.printf "Looking at malloc of (%s).  Subtype is malloc: %b\n" (synth_type_to_string (Pointer(p))) (pointer_is_malloc)
				else ()
				in
				let sub_base_type = match base_type with
				| Pointer(sub_p) -> sub_p
				| _ -> assert false (* Don't think this is possible right now? *)
				in
				let subtypdef = 
					let subdef = cxx_definition_synth_type_to_string options typemap None escapes p sub_base_type context (name ^ "_sub_elem") None in
					subdef ^ "\n" ^
					(* TODO --- pretty sure this is broken?  What should it be? *)
					"*" ^ name ^ " = " ^ (name ^ "_sub_elem") ^ ";"
				in
				alloc ^ "\n" ^ subtypdef
		| other ->
				let () = if options.debug_generate_malloc then
					let () = Printf.printf "Type (%s) did not require full sub-malloc, using top-level malloc.  " (synth_type_to_string other) in
					Printf.printf "That alloc is %s\n" (alloc)
				else ()
				in
				(* Don't need to malloc anything *)
				alloc
	)


let cxx_names_to_type_definition variable_map names =
    List.map names ~f:(fun name -> (cxx_type_signature_synth_type_to_string (Hashtbl.find_exn variable_map name)) ^ " " ^ name)

let rec cxx_generate_from_gir options (typemap: typemap) gir =
    match gir with
    | Definition(nref, escapes, definition_type, defvalue) ->
			let alignment = Hashtbl.find typemap.alignment_map (cxx_gir_name_to_string nref) in
			let definition_type = match definition_type with
			| Some(x) -> x
			| None -> raise (CXXGenerationException "Error: untyped def: see generate_gir:get_definition_type_for ")
			in
            cxx_definition_synth_type_to_string options typemap alignment escapes definition_type definition_type "" (cxx_gir_name_to_string nref) defvalue
    | Sequence(girlist) ->
            String.concat ~sep:";\n\t" (List.map girlist ~f:(cxx_generate_from_gir options typemap))
    | Assignment(fromv, tov) ->
			let pre_from_code, fromv_name, fromv_type = cxx_generate_from_lvalue typemap fromv in
			let pre_to_code, tov_name = cxx_generate_from_rvalue typemap tov in

			(* let () = Printf.printf "Looking at variable with name %s\n" (tov_name) in
			let () = Printf.printf "Typemap has keys %s\n" (String.concat (Hashtbl.keys typemap.variable_map)) in *)
			let assignment =
				match fromv_type with
				(* TODO --- should this also do a stringcopy if it's casting? *)
				(* I don't really know --- think that's not
				generated right now. *)
				| Some(String) ->
						(* So, in C, strings can't just
						use the = operator to properly copy.
						Instead, we've got an instrinsic
						strcopy function we can generate.  *)
						"facc_strcopy(" ^ fromv_name ^ ", " ^ tov_name ^ ");"
				| _ ->
						(* If the types aren't strings, we
						can just use the '=' operator as normal.
						Other non-trivial copies are represented
						internally within the compiler. *)
						fromv_name ^ " = " ^ tov_name ^ ";"
			in
			(trim pre_from_code) ^ (trim pre_to_code) ^ assignment
    | LoopOver(gir, indvariable, loopmax) ->
            let indvar_name = (cxx_gir_name_to_string indvariable) in
            let pre_loopmax_code, loopmax_name = (cxx_generate_from_expression typemap loopmax) in
			(trim pre_loopmax_code) ^
            "for (int " ^ indvar_name ^ " = 0; " ^ indvar_name ^ " < " ^ loopmax_name ^ "; " ^ indvar_name ^ "++) {\n\t\t" ^
            (cxx_generate_from_gir options typemap gir) ^
            "\n\t}"
    | Expression(expression) ->
			let pre_code, expr_code = (cxx_generate_from_expression typemap expression) in
			String.concat [(trim pre_code); expr_code]
	| IfCond(cond, iftrue, iffalse) ->
			let pre_cond_code, cond_code = cxx_generate_from_conditional typemap cond in
			let true_code = cxx_generate_from_gir options typemap iftrue in
			let false_code = cxx_generate_from_gir options typemap iffalse in
			(trim pre_cond_code ^ "\n") ^
			"if (" ^ cond_code ^ ") {\n" ^
			true_code ^ "\n} else {\n" ^
			false_code ^ "\n}"
	| Free(v) ->
			(* Depending on the mode, this may not expand into anything.  *)
			let precode, postcode, typ = cxx_generate_from_variable_reference typemap true v in
			let free =
				match options.compile_settings.allocation_mode with
					| StaticAllocationMode | StackAllocationMode -> ""
					| HeapAllocationMode ->
							if is_malloc_type typemap (Option.value_exn typ) then
								"facc_free(" ^ (postcode) ^ ")"
							else
								""
			in
			(* note that precode should basically always be empty.  *)
			precode ^ "\n" ^ free
	| Return(v) ->
			let precode, expr_code = cxx_generate_from_expression typemap v in
			(trim precode ^ "\n") ^
			"return " ^ (expr_code) ^ ";"
	| FunctionDef(name, args, body, fun_typtable) ->
			(* Not doing definitions as lambdas,
			   so it wont make much sense if this is
			   part of a sequence.  *)
			let funtyp = match Hashtbl.find_exn fun_typtable (gir_name_to_string name) with
			| Fun(f, t) -> t
			| _ -> raise (CXXGenerationException "Unexpected non-function type function type!") in
			let funtyp_name = cxx_type_signature_synth_type_to_string funtyp in
			let args_strings = List.map args ~f:gir_name_to_string in
			let args_def = String.concat ~sep:", " (cxx_names_to_type_definition fun_typtable args_strings) in
			(* Suppose we probably shouldn't just pass the old lenvar bindings
				into this one.. *)
			let emptymap = Hashtbl.create (module String) in
			let fun_typemap = {
				variable_map = fun_typtable;
				classmap = typemap.classmap;
				(* No particular variable alignments for a function.  *)
				alignment_map = emptymap;
				original_typemap = None;
			} in
			let body_code = cxx_generate_from_gir options fun_typemap body in
			String.concat [
				funtyp_name; " "; (cxx_gir_name_to_string name);
				" ("; args_def; ") {\n"; body_code;
				"\n}\n"
			]
    | EmptyGIR -> ";"

(* BEcahse C++ is an imperative language, this stuff
	sometimes needs linearlization --- so these return
	'pre' code and then the expression, where
	pre code is stuff that should be run before, e.g.
	to assign to the right variables.  *)
and cxx_generate_from_lvalue typemap lvalue =
    match lvalue with
    | LVariable(nref) ->
			let pre, ref, typ = (cxx_generate_from_variable_reference typemap false nref) in
			pre, ref, typ

(* So this find_type parameter is a bit of a hack.

THe post-behavioural synthesizer generates some 'function'
(i.e. #define) parameters that are not 'real' variables
because they are concated like strings by the #define.

As a result, they don't exist in the typemap.

Perhaps a less hacky way of doing this owuld be to properly
type those, but that isn't great either, since you'd end
up with types for invalid variables in the typemap. I'd argue
that's more fundamental.. Anyway. It's a bit shit here,
like 'some' variable references don't have to have types.
*)
and cxx_generate_from_variable_reference typemap find_type vref =
	match vref with
	| Variable(Name(nm)) ->
			(* let () = Printf.printf "Variable is %s%!\n" (nm) in *)
			let typ =
				if find_type then
					if (String.compare nm "") = 0 then
						(* Empty is used as a placeholder by the behavioural synthesizer.  *)
						(* Unit seems like the best type to give that --- it obviously can't be passed
						to functions, but it is used to be passed to #defines.  *)
						Some(Unit)
					else
						Some(Hashtbl.find_exn typemap.variable_map nm)
				else
					None
			in
			"", nm, typ
	| MemberReference(structref, member) ->
			(* Need to get the subtype to support correct pointer/index generation.  *)
			let struct_pre_code, struct_reference, structtype_opt = cxx_generate_from_variable_reference typemap true structref in
			let structtype = Option.value_exn structtype_opt in
			let classname = match structtype with
				| Pointer(Struct(nm)) -> nm
				| Struct(nm) -> nm
				| other -> raise (CXXGenerationException "Unexecpted member reference")
			in
			let class_typemap = get_class_typemap (Hashtbl.find_exn typemap.classmap classname) in
			let member_type = Hashtbl.find_exn class_typemap (gir_name_to_string member) in
			let op =
				match structtype with
				| Pointer(v) -> "->"
				| Struct(v) -> "."
				| other -> raise (CXXGenerationException "Unexpected member reference")
			in
			struct_pre_code, struct_reference ^ op ^ (cxx_gir_name_to_string member), Some(member_type)
	| IndexReference(arr, ind) ->
			let pre_code, ind_reference = cxx_generate_from_expression typemap ind in
			let arr_pre_code, arr_reference, arr_reference_typ = cxx_generate_from_variable_reference typemap find_type arr in
            let reftyp = match arr_reference_typ with
            | Some(Array(sty, _)) -> Some(sty)
            | Some(other) -> raise (CXXGenerationException "Unexpected array reference")
			| None ->
					let () = assert (not (find_type)) in
					None
            in
			arr_pre_code ^ "\n" ^ pre_code, arr_reference ^ "[" ^ ind_reference ^ "]", reftyp
	| Constant(synth_value) ->
			(* TODO --- properly support more complex synth values, e.g. arrays or structs.  *)
			(* Has empty pre code *)
			"", (cxx_generate_from_synth_value typemap synth_value), Some(synth_value_to_type synth_value)
	| Cast(vref, typ) ->
			let precode, refcode, original_type = cxx_generate_from_variable_reference typemap true vref in
			precode, "(" ^ (cxx_type_signature_synth_type_to_string typ) ^ ")" ^ refcode, Some(typ)

and cxx_generate_from_rvalue typemap rvalue =
    match rvalue with
    | Expression(expr) -> (cxx_generate_from_expression typemap expr)
and cxx_generate_from_expression typemap expr =
    match expr with
    | VariableReference(nref) ->
			let pre, post, typ = cxx_generate_from_variable_reference typemap false nref in
			pre, post
    | FunctionCall(fref, vlist) ->
			(* let () = Printf.printf "fref code is %s\n" (cxx_generate_from_function_ref fref) in *)
			let pre_args_code, args_list = (cxx_generate_from_vlist typemap vlist) in
            pre_args_code, (cxx_generate_from_function_ref fref) ^ "(" ^ args_list ^ ");"
	| GIRMap(vfrom, value_pairs_list) ->
			(* I can't remember what types are supported in
			   the switch statement, but other things, e.g.
			   array lookups may be faster depending on the
			   type. *)
			let temp_name = generate_generic_tmp () in
			let vfrom_reference = gir_name_to_string vfrom in
			(* We need a default value for this thing, so just pick
			the first one from the map.  *)
			let first_source, default_value = List.hd_exn value_pairs_list in
			let vtype = Hashtbl.find_exn typemap.variable_map (gir_name_to_string vfrom) in
			let deftyp = cxx_type_signature_synth_type_to_string vtype in
			(* TODO --- Should we crash when a value outside the rnage
			is presented?  Clearly that doesn't make sense from an impl
			perspective (or does it?) but it certianly makes sense from
			a debugging perspective.  *)
			(* Pre-assign code *)
			match vtype with
			| (Float16 | Float32 | Float64) ->
				(* for FP types, we can't use a switch in c *)
					(* We should really do better than initializing to a Nan, but really, to generate a map, a range
					has to have been promised. *)
			   deftyp ^ " " ^ temp_name ^ " = nanf(\"0\");\n" ^ (
				   String.concat (
                       List.map value_pairs_list ~f:(fun (vfrom, vto) ->
						   "if (FLOAT_EQUAL(" ^ (synth_value_to_string vfrom) ^ ", " ^ vfrom_reference ^ ")) {" ^ temp_name ^ " = " ^ (synth_value_to_string vto) ^ "; }"
					   )
				   )
			   ),
			   temp_name
			| _ -> (* all other types can use switch-case. *)
				deftyp ^ " " ^ temp_name ^ " = " ^ (synth_value_to_string default_value) ^ "; \n" ^
				"switch (" ^ vfrom_reference  ^ ") {\n" ^
				(
					String.concat (
						List.map value_pairs_list ~f:(fun (vfrom, vto) ->
							"case " ^ (synth_value_to_string vfrom) ^ ": \n" ^
							temp_name ^ " = " ^ (synth_value_to_string vto) ^ "; break;\n"
						)
					)
				) ^ "\n}\n",
				(* Assign code is just a ref to the temp_name *)
				temp_name
and cxx_generate_from_function_ref fref =
    match fref with
    | FunctionRef(nref) -> (cxx_gir_name_to_string nref)
and cxx_generate_from_vlist typemap vlist =
    match vlist with
    | VariableList(nrefs) ->
		let pre_code, refs, types = Utils.unzip3 (List.map nrefs ~f:(cxx_generate_from_variable_reference typemap false)) in
        (String.concat ~sep:"\n" pre_code, String.concat ~sep:", " refs)
and cxx_generate_from_conditional typemap cond =
	match cond with
	| Check(vref, comparator) ->
			(* Unary comparators are represented
			as functions, e.g. REPR(var) *)
			let vref_pre_code, vref_ref, vref_type = cxx_generate_from_variable_reference typemap false vref in
			vref_pre_code, (cxx_generate_from_unary_comparator comparator) ^ "(" ^
			vref_ref ^ ")"
	| Compare(vref1, vref2, oper) ->
			(* Again, use a function-like structure, relying
			on #defines in the clib to provide the appropriate
			definitions.  *)
			let vref1_pre_code, vref1_ref, vref1type = cxx_generate_from_variable_reference typemap false vref1 in
			let vref2_pre_code, vref2_ref, vref2type = cxx_generate_from_variable_reference typemap false vref2 in
			vref1_pre_code ^ "\n" ^ vref2_pre_code ^ "\n",
			(cxx_generate_from_binary_comparator oper) ^ "(" ^
			vref1_ref ^
			", " ^ vref2_ref ^ ")"
	| CondOr(c1, c2) ->
			let c1_pre, c1_code = cxx_generate_from_conditional typemap c1 in
			let c2_pre, c2_code = cxx_generate_from_conditional typemap c2 in
			c1_pre ^ "\n" ^ c2_pre, "(" ^ c1_code ^ ") || (" ^
			c2_code ^ ")"
	| CondAnd(c1, c2) ->
			let c1_pre, c1_code = cxx_generate_from_conditional typemap c1 in
			let c2_pre, c2_code = cxx_generate_from_conditional typemap c2 in
			c1_pre ^ "\n" ^ c2_pre,
			"(" ^ c1_code ^
			") && (" ^ c2_code ^ ")"
and cxx_generate_from_unary_comparator comparator =
	match comparator with
	(* Defed in synthesizer.h *)
	| PowerOfTwo -> "POWER_OF_TWO"
and cxx_generate_from_binary_comparator comparator =
	match comparator with
	| GreaterThan -> "GREATER_THAN"
    | GreaterThanOrEqual -> "GREATER_THAN_OR_EQUAL"
	| LessThan -> "LESS_THAN"
    | LessThanOrEqual -> "LESS_THAN_OR_EQUAL"
	(* TODO --- perhaps a def of this depends on the type? *)
	| Equal -> "PRIM_EQUAL"
    | FloatEqual -> "FLOAT_EQUAL"

(* Some variables are 'dead in', i.e. they don't need to be assigned
to, just allocated.  This does just the allocation part :) *)
let rec generate_empty_assign_to assname typ =
    match typ with
    (* May have to have a special case for some things here?  not sure,
    C is actually pretty good with defines-in-place. *)
    | _ ->
			let type_sig = cxx_type_signature_synth_type_to_string typ in
            type_sig ^ " " ^ assname ^ ";"

let rec generate_assign_to typemap assname fieldname typ join_op json_ref declare_type =
	let json_ref =
		match fieldname with
		(* Some refs, e.g. after arrays are actually impllicit.  *)
		| None -> json_ref
		| Some(n) -> json_ref ^ "[\"" ^ n ^ "\"]"
	in
	match typ with
	| Array(artyp, _) ->
            let artypstr = cxx_type_signature_synth_type_to_string artyp in
			let temp_assname = generate_input_tmp() in
			let vecname = temp_assname ^ "_vec" in
			let resdef = "std::vector<" ^ artypstr ^ "> " ^ vecname ^ ";" in
			let loop_header = "for (auto& elem : " ^ json_ref ^ ") {" in
			let recursed_code = generate_assign_to typemap (temp_assname ^ "_inner") None artyp "." "elem" true in
            let in_loop_assign =
                vecname ^ ".push_back(" ^ temp_assname ^ "_inner);" in
			let end_loop = "}" in
			(* Assign back to a pointer, since that's what we are treating arrays
			as --- may have to put some thought into indexed classes too.  Not
			100% how to go about that right now.  *)
			let post_loop_assign = assname ^ " = &" ^ vecname ^ "[0];" in
			let post_loop = if declare_type then artypstr ^ " *" ^ post_loop_assign else post_loop_assign in
			resdef ^ "\n" ^ loop_header ^ "\n" ^ recursed_code ^ "\n" ^ in_loop_assign ^ "\n" ^ end_loop ^
			"\n" ^ post_loop
	| Pointer(stype) ->
			let pointer_typ_str = cxx_type_signature_synth_type_to_string stype in
			let sub_variable = assname ^ "_pointer" in
			let recursed_code = generate_assign_to typemap sub_variable None stype "->" json_ref true in
			let assignment = assname ^ " = &" ^ sub_variable ^ ";" in
			let declare_and_assign = if declare_type then pointer_typ_str ^ " *" ^ assignment else assignment in
			(* Pointers obviously aren't stored as such in JSON files.   But
			 they can be stack allocated here. *) recursed_code ^ "\n" ^
			declare_and_assign
	| Struct(sname) ->
			(* Get the members we need to fill, and
			   then get the values.  *)
			let structmeta = Hashtbl.find_exn typemap.classmap sname in
			let struct_typemap = get_class_typemap structmeta in
			let members = get_class_fields structmeta in
			(* Pair those members with their types *)
			let memtypes = List.map members ~f:(fun mem ->
				mem, (Hashtbl.find_exn struct_typemap mem)) in
			(* Recurse to get the values *)
			let members_assigns =
				(* The __ could obviously create aliasing
				   issues --- perhaps can just require that
				   the iospe doesn't contain aliases like this

				   The alternative would be to escape all the
				   occurances of __ with something else.  *)
				List.map memtypes ~f:(fun (mem, memtyp) -> generate_assign_to typemap (assname ^ "__" ^ mem) (Some(mem)) memtyp "." json_ref true) in
			let memass_names = List.map members ~f:(fun m -> (assname ^ "__" ^ m)) in
			(* Now, build the struct *)
			let class_assign = if is_class structmeta then
				(* is a class *)
				(* We assume that the class has a 'simple' constructor *)
				assname ^ "(" ^ (String.concat ~sep:", " memass_names) ^ ");"
			else 
				(* is a struct *)
				assname ^ " = { " ^ (String.concat ~sep:", " memass_names) ^ "};"
			in
			let class_define_and_assign =
				if declare_type then sname ^ " " ^ class_assign else class_assign in
			String.concat ~sep:"\n" (members_assigns @ [
				class_define_and_assign
			])
	| String ->
			(* Strings need a special type handler because nlohmann json deals
			oddly with them.  *)
			let type_sig = cxx_type_signature_synth_type_to_string String in
			let assignment = assname ^ " = strdup(" ^ json_ref ^ ".get<std::string>().c_str());" in
			let declare_and_assign = if declare_type then type_sig ^ " " ^ assignment else assignment in
			declare_and_assign
	| Int8 ->
			(* Raw chars have the same problem as strings.  *)
			let type_sig = cxx_type_signature_synth_type_to_string Int8 in
			let assignment = assname ^ " = " ^ json_ref ^ ".get<char>();" in
			let declare_and_assign = if declare_type then type_sig ^ " " ^ assignment else assignment in
			declare_and_assign
	| _ ->
			let type_sig = cxx_type_signature_synth_type_to_string typ in
			let assignment = assname ^ " = " ^ json_ref ^ ";" in
			let declare_and_assign = if declare_type then type_sig ^ " " ^ assignment else assignment in
			declare_and_assign

(* Given the Input typespec, generate some code that reads
those IO values from a JSON file and puts them into values.
Returns boht the code, and a list of function args to apply.
*)
let rec generate_input_assigns options (typemap: typemap) inps livein liveout json_ref =
	(* Note that inps != livein @ liveout, because there may be duplicates,
	and global variables that are livein/liveout may not be in the function
	arguments.  *)
	(* If there are infered types, use the original typemap.  *)
	let iotypemap = match typemap.original_typemap with
			| Some(t) -> t
			(* If there is no original typemap, there might
			e.g. not have been any structural inference
			or this could be json gen.

			Note that "has structural inference => this is none"
			is false.  *)
			| None -> typemap
	in
	let asses = List.map livein ~f:(fun inp ->
		(* THis hsould be easy -- if it's not an array, then
			just load the value -- if it is an array, then
			do the complicated ararys stuff and recurse. *)
		let typ = Hashtbl.find_exn iotypemap.variable_map inp in
		(* only declare the variable if it's in the input list to
		the function--- otherwise, assume it's a global variable, that
		shouldn't be declared.  *)
		let should_declare = Utils.strings_any_equal [inp] inps in
		(* Note that because this uses a C++ vector to build
		directly from the file, we don't need to use any length
		parameter adjustments (since those are implicit).  *)
		generate_assign_to iotypemap inp (Some(inp)) typ "." json_ref should_declare
	) in
	(* There are some variables that are 'dead' in, i.e. the
	need to be allocated (e.g. output arrays), but they don't
	need to be filled.  Call those 'deadin' values. *)
	let deadin = set_difference (fun x -> fun y -> (String.compare x y) = 0) (livein @ liveout) livein in
	(* No matter the assignment mode, read defs can't be static,
	or rather, I'm not going to implement that.
	THis is just testing code, so doesn't have anything
	to do with the real defs.  *)
	let deadin_assignment_options = {
		options with compile_settings = {
			allocation_mode = StackAllocationMode
		}
	}
	in
    let deadin_defs = List.map deadin ~f:(fun inp ->
		let infered_type = Hashtbl.find_exn typemap.variable_map inp in
        let typ = Hashtbl.find_exn iotypemap.variable_map inp in
		let alignment = Hashtbl.find iotypemap.alignment_map inp in
		(* Things that go into the API are assumed to be dead-in.  *)
		cxx_definition_synth_type_to_string deadin_assignment_options typemap alignment false typ infered_type "" inp None
    ) in
	(* Hope and pray we don't end up needing to topo sort
	this shit. *)
	let all_asses = asses @ deadin_defs in
	((String.concat ~sep:"\n" all_asses), String.concat ~sep:", " inps)

(* Given a list of the liveout vars, produce an asssignment
that produces the output JSON.*)
let rec generate_output_assigns options typemap program types outvars outprefix outjson =
	let asses = List.map (List.zip_exn outvars types) ~f:(fun (out, typ) ->
		let defcode, vname = (generate_output_assign options typemap program typ out outprefix ".") in
		(* Defcode is the code to define any intermediate values needed. *)
		defcode ^ "\n" ^
		outjson ^ "[\"" ^ out ^ "\"] = " ^ vname ^ ";"
	) in
	(String.concat ~sep:"\n" asses)

(* note that this is a slightly confusing definition, because we are
trying to input from the 'out' variable into the json, not assign
to it.  *)
and generate_output_assign options typemap program (infered_typ, typ) out out_prefix operator =
	let () = if options.debug_generate_code then
		Printf.printf "Generating outupt for %s\n" (out)
	else () in
	match typ with
	| Array(artyp, adim) ->
		let dim_ratio_modifier =
			if use_dim_ratio_modifier adim then
				compute_dimension_ratio_modifier typ infered_typ
			else (fun v -> v)
		in
		let _ = match infered_typ with
		| Array(sty, sdim) -> sdim
		(* Ideally, we could handle this, eg. in infering
		structs over fixed-length arrays, but I'm not
		actually sure what to do here. (maybe nothingish
		is OK? *)
		| _ -> raise (CXXGenerationException "Infered non-array over array." )
		in
		(* Needs to go back to a std::vector-based type *)
		let artypname = cxx_vectors_type_signature_synth_type_to_string artyp in
		let outtmp = generate_out_tmp () in
        let ivar = generate_ivar_tmp() in
		(* Note that using the out_prefix here is a bit simplistic: it restricts the use
		   of length variables to those that exist in the context of the struct.   This
		   isn't a problem right now, but one could imagine odd situations where
		   variable X has length *)
		let length = cxx_dimtype_to_name typemap out_prefix adim in
		let vecres = "std::vector<json> " ^ outtmp ^ ";" in
		(* TODO--- need to actually properly handle multi-dimensions here.  *)
		(* (Array indexing like this won't work for the C view of the world --probaly
		need multipied indexes or some shit. ) *)
		let assloop_header = "for (unsigned int " ^ ivar ^ " = 0; " ^ ivar ^ " < " ^ (dim_ratio_modifier length) ^ "; " ^ ivar ^ "++) {" in
		let newout = generate_out_tmp() in
		let newout_assign = artypname ^ " " ^ newout ^ " = " ^ (out_prefix ^ out) ^ "[" ^ ivar ^ "];" in
		let assbody, assresvar = generate_output_assign options typemap program (artyp, artyp) newout "" "." in
		(* Add to the array. *)
		let inloopassign = outtmp ^ ".push_back(" ^ assresvar ^ ");" in
		let loop_end = "}" in
		(String.concat ~sep:"\n" [
			vecres; assloop_header; newout_assign; assbody; inloopassign; loop_end
		], outtmp)
	| Pointer(styp) ->
		let infered_subtyp =
			match infered_typ with
			| Pointer(st) -> st
			(* Again -- what do we want to do here? *)
			| _ -> raise (CXXGenerationException "Unexpected type infered over pointer")
		in
		let ptyp = cxx_vectors_type_signature_synth_type_to_string styp in
		let outtmp = generate_out_tmp() in
		let outtmp_assign = ptyp ^ " " ^ outtmp ^ " = " ^ "*" ^ out ^ ";" in
		let sub_ass, assresvar = generate_output_assign options typemap program (infered_subtyp, styp) outtmp out_prefix "." in
		(String.concat ~sep:"\n" [
			outtmp_assign; sub_ass
		], assresvar)
	| Struct(n) ->
		let _ = match infered_typ with
		(* Don't know what to do here. *)
		(* currently don't do this anyway? *)
		| Struct(n') -> assert ((String.compare n n') = 0)
		| _ ->
				let () = Printf.printf "Inferedtyp is %s, selftyp is %s\n" (synth_type_to_string infered_typ) (synth_type_to_string typ) in
				assert false
		in
		let json_tmp = generate_out_tmp () in
		let defn = "json " ^ json_tmp ^ ";" in
		let structdefn = Hashtbl.find_exn typemap.classmap n in
		let sub_assigns = List.map (get_class_fields structdefn) ~f:(fun a -> a) in
		(* Need the unprefixed assigns so we can get their types from the typemap.  *)
		let unprefixed_assigns = get_class_fields structdefn in
		let sub_typemap = get_class_typemap structdefn in
		let sub_types = List.map unprefixed_assigns ~f:(fun ass ->
			let restyp = Hashtbl.find_exn sub_typemap ass in
			(restyp, restyp)
		) in
		let asscode = generate_output_assigns options typemap program sub_types sub_assigns (out_prefix ^ out ^ operator) json_tmp in
		String.concat ~sep:"\n" [defn; asscode], json_tmp
	| _ ->
		(* We can literally just put the variable name.  *)
		"", out_prefix ^ out

let cxx_type_from_returnvar typemap retvar =
	match retvar with
	| [] -> "void", ""
	| [x] -> cxx_type_signature_synth_type_to_string (Hashtbl.find_exn typemap x), x
	| _ -> raise (CXXGenerationException "C++ Doesn't hangle multiple returns")

(* Imports needed for the running infrastructure.  *)
let otherimports = String.concat ~sep:"\n" [
    "#include<vector>"; "#include<nlohmann/json.hpp>";
    "#include<fstream>"; "#include<iomanip>";
	"#include<clib/synthesizer.h>";
    "#include<time.h>";
	"#include<math.h>";
	"#include<iostream>";
    "char *output_file; ";
	"char *pre_accel_dump_file; // optional dump file. ";
	"using json = nlohmann::json;"; (* Not strictly an include I suppose.  *)
	"const char* __asan_default_options() { return \"detect_leaks=0\"; }" (* Disable leak as code 
	is allowed to leak if the user code did.  *)
]

(* Generate a dump function that dumps vnames into the file stored in
the variable filename.  argnames must be a superset of vnames,
and should contain auxiliary things that don't need to be dumped,
e.g. length parameters.  *)
let generate_dump_function options (typemap: typemap) (program: program) filename argnames vnames funname =
	let header = "void " ^ funname ^ "(" ^ (String.concat ~sep:", " (cxx_names_to_type_definition typemap.variable_map argnames)) ^ ") {\n" in
	let json_out_name = "output_json" in
	let write_json_def = "    json " ^ json_out_name ^ ";" in
	(* So these type pairs are a bit of a hack here --- they
	are what is needed, but the generate_output_assigns is
	disturbingly coupled with what types might be infered. *)
	(* Anyway, it's only here to allow the correct array lengths
	to be used for the output writing code.  *)
	let types = List.map vnames ~f:(fun i ->
			(Hashtbl.find_exn program.typemap.variable_map i,
			Hashtbl.find_exn typemap.variable_map i)
		) in
	let gen_results = generate_output_assigns options typemap program types vnames "" json_out_name in
	let ofstream_create = "std::ofstream out_str(" ^ filename ^ "); " in
	let ofstream_write = "out_str << std::setw(4) << " ^ json_out_name ^ " << std::endl;" in
	let tail = "}" in
	String.concat ~sep:"\n" [
		header; write_json_def; gen_results; ofstream_create; ofstream_write; tail
	]

let generate_user_visible_function (program: program) =
	let origmap = Option.value_exn program.typemap.original_typemap in
	let rettype, _ = cxx_type_from_returnvar origmap.variable_map program.returnvar in
	let header = 
		rettype ^ " " ^ program.generated_funname ^ "("
		^ (String.concat ~sep:", " (cxx_names_to_type_definition origmap.variable_map program.funargs)) ^ ") {"
	in
	let cast_args =
		List.map program.funargs ~f:(fun arg ->
			(* We have to generate this because the structure
			inference tool might infer different types --
			we need to cast those appropriately.  *)
			let cast_type = cxx_type_signature_synth_type_to_string (Hashtbl.find_exn program.typemap.variable_map arg) in
			"(" ^ cast_type ^ ") " ^ arg
		)
	in
	let call_args =
		(String.concat ~sep:", " cast_args)
	in
	let returncast, useret = match program.returnvar with
	| [] -> "", "" (* No cast on void returns.  *)
	| _ -> "(" ^ rettype ^ ")", "return "
	in
	let call = useret ^ returncast ^ program.generated_funname ^ "_internal(" ^ call_args ^ ");" in
	String.concat ~sep:"\n" [
		header; call; "}"
	]

(* Given the IOSpec, generate the main function required to actually run this
thing --- it should respect the specification for taking in
as args the input JSON file, and putting the outputs of the function
in the output JSON file.  *)
let cxx_main_function options dump_intermediates returntype (program: program) =
	let json_var_name = "input_json" in
	let header = "int main(int argc, char **argv) {" in
	let argreader =      "    char *inpname = argv[1]; " in
	let resdump =        "    output_file = argv[2]; " in
	let pre_accel_dump = if dump_intermediates then
		"    pre_accel_dump_file = argv[3];"
	else
		(* Only load the pre_accel_dump_file frmo the args
		if we are using dump intermediates -- understanably
		in the final output we don't want to have a random
		dumpfile created... *)
		""
	in
	let load_file =      "    std::ifstream ifs(inpname); " in
	let load_json =      "    json " ^ json_var_name ^ " = json::parse(ifs);" in
	let parse_args, argnames = generate_input_assigns options program.typemap program.funargs program.livein program.liveout json_var_name in
	(* TODO -- need to handle non-void call_funcs here.  *)
	let pre_timing_code =
		if options.generate_timing_code then
            "clock_t begin = clock();"
        else
            ""
    in
	let result_assignment =
		if (String.compare "void" returntype) = 0 then
			(* No return values, so just leave as is.  *)
			""
		else
			(* We assume only a single returnvar here.  *)
			let () = assert (List.length program.returnvar = 1) in
			if (List.mem (program.funargs) (List.hd_exn program.returnvar) ~equal:Utils.string_equal) then
				(* If the returnvar is one of the funargs, then we don't have
				to redefine it.  It's not clear that this is 100% correct,
				but this is the only case in which the name of the returnvar
				has any meaning -- so, I suppose I define this as the semantics
				of what assining the name of he returnvar to be one of the other
				vars.  Intended use case is e.g. if usercode returns one of
				the things passed as an argument.   *)
				(List.hd_exn program.returnvar) ^ " = "
			else
				returntype ^ " " ^ (String.concat ~sep:"" program.returnvar) ^ " = "
	in
	let call_func = result_assignment ^ program.generated_funname ^ "(" ^ argnames ^ ");" in
    let post_timing_code =
        if options.generate_timing_code then
			"clock_t end = clock();"
        else
            ""
    in
	(* Generate a function that has the types of the original
	   user code before any type inference etc.  *)
	let user_visible_function =
		generate_user_visible_function program
	in
    let timing_print_code =
        if options.generate_timing_code then
            "std::cout << \"Time: \" << (double) (end - begin) / CLOCKS_PER_SEC << std::endl;
std::cout << \"AccTime: \" << (double) AcceleratorTotalNanos / CLOCKS_PER_SEC << std::endl;"
        else
			""
    in
	let origmap = Option.value_exn program.typemap.original_typemap in
	let writeout_args = (Utils.remove_duplicates Utils.string_equal (program.funargs @ program.returnvar)) in
	let output_args = (Utils.remove_duplicates Utils.string_equal (program.liveout @ program.returnvar)) in
	let output_writing_function =
		generate_dump_function options origmap program "output_file" (writeout_args) (output_args) "write_output"
	in
	let accelerator_timing_functions =
		if options.generate_timing_code then
			accelerator_timer_functions
		else
			""
	in
	let output_write_call =
        "write_output(" ^ (String.concat ~sep:", " writeout_args) ^ ");"
	in
	let tail = "}" in
	String.concat ~sep:"\n" [accelerator_timing_functions; output_writing_function],
	user_visible_function,
	String.concat ~sep:"\n" [header; argreader; resdump; pre_accel_dump; load_file; load_json; parse_args;
    pre_timing_code; call_func; post_timing_code; timing_print_code;
	output_write_call; tail]

let generate_cxx (options: options) (apispec: apispec) (iospec: iospec) dump_intermediates (program: program) =
    let skeleton_description = match program.original_pairs with
    | None -> "// No skeleton pairs specified"
    | Some(s) -> cxx_generate_skeleton_description s
    in
	let typemap_description = cxx_generate_typemap_description program.typemap in
    (* C++ only allows for single return values.  *)
    (* This could be ammened to auto-add a struct,
    but can't imagine we'd need that.  *)
    let (function_type, outv) =
		cxx_type_from_returnvar program.typemap.variable_map program.returnvar
    in
	let intermediate_dump_function =
		if dump_intermediates then
			[generate_dump_function options program.typemap program "pre_accel_dump_file" apispec.livein apispec.livein options.pre_accel_dump_function]
		else []
	in
	(* Generate the required helper functions.  *)
	let helper_funcs = String.concat ~sep:"\n" (
		(* Note that the typemap and lenvar bindings aren't
		(/shouldn't be) used in this call anyway, they're replaced
		by the ones in the program unit. *)
		(List.map program.fundefs ~f:(cxx_generate_from_gir options program.typemap))
		@ (intermediate_dump_function)
	) in
    (* Generate the function header *)
    let function_header =
        function_type ^ " " ^ program.generated_funname ^ "_internal(" ^
        (String.concat ~sep:"," (cxx_names_to_type_definition program.typemap.variable_map program.funargs)) ^
        ") {" 
    in
    (* Generate the actual program --- need to include e.g. any range programs
	   or behavioural programs.  *)
	let program_gir = generate_single_gir_body_from options apispec dump_intermediates program in
	(* TODO -- probabloy need to augment the typemap and lenvar bindings with
	those from the post-behaviour/range checker.  *)
    let program_string = cxx_generate_from_gir options program.typemap program_gir in
	let program_includes =
		String.concat ~sep:"\n" (generate_includes_list_from program) in
	let ioimports = cxx_generate_imports iospec.required_includes in
    let apiimports = cxx_generate_imports apispec.required_includes in
    let function_end = "}" in
	let main_helper_funcs, post_accel_def_funcs, main_func = cxx_main_function options dump_intermediates function_type program in
    (* Generate the whole program.  *)
	String.concat ~sep:"\n" [skeleton_description; typemap_description; program_includes; ioimports; apiimports; otherimports; main_helper_funcs; helper_funcs; function_header; program_string; function_end; post_accel_def_funcs; main_func]
    (* TODO --- need to include a bunch of unchanging crap, e.g. 
    arg parsing.   I expect that to change /a little/ with
    the argtypes but not much.  *)

let generate_code (options: options) apispec (iospec: iospec) dump_intermediates (programs: program list) =
	let codes = match options.target with
	| CXX -> List.map programs ~f:(fun prog ->
			(prog, generate_cxx options apispec iospec dump_intermediates prog)
	)
	in
	let () =
		if options.dump_generate_program then
            Printf.printf "Generated codes are %s\n" (String.concat ~sep:"\nNEWPROGRAM\n\n" (List.map codes ~f:(fun (_, c) -> c)))
        else () in
    codes
