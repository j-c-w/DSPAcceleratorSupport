open Core;;
open Options;;
open Spec_definition;;
open Spec_utils;;
open Skeleton;;
open Skeleton_definition;;
open Skeleton_utils;;
open Gir;;
open Gir_clean;;
open Gir_utils;;
open Gir_topology;;
open Utils;;
open Builtin_conversion_functions;;
open Program;;
open Range;;

exception GenerateGIRException of string

let induction_variable_count = ref 0
let new_induction_variable () =
    let () = induction_variable_count := !induction_variable_count + 1 in
    Name("i" ^ (string_of_int !induction_variable_count))

let variable_count = ref 0
let new_variable () =
    let () = variable_count := !variable_count + 1 in
    Name("v_" ^ (string_of_int !variable_count))

let conversion_function_count = ref 0
let new_conversion_function () =
    let () = conversion_function_count := !conversion_function_count + 1 in
    Name("conversion" ^ (string_of_int !conversion_function_count))

let to_gir_name_list_list slist_list: gir_name list list =
    List.map slist_list ~f:(fun slist ->
        List.map slist ~f:(fun s ->
            Name(s)
        )
    )

let generate_gir_name_for nref =
    match nref with
    | AnonymousName -> None
    | Name(n) -> Some(Variable(Name(n)))
    | StructName(names) ->
            let names = List.map names ~f:(fun n -> match n with
            | Name(n) -> n
            | _ -> raise (GenerateGIRException "UNexepcted!")
            ) in
            let n, ns = match names with
            | n :: ns -> n, ns
            | _ -> raise (GenerateGIRException "Empyt struct name!") in
            Some(List.fold
                ns
                ~init:(Variable(Name(n)))
                ~f:(fun namesofar nextname ->
                    MemberReference(namesofar, Name(nextname))))

let generate_gir_names_for nrefs =
    List.map nrefs ~f:generate_gir_name_for

(* Given a type typ, and a varaible from_basename that has
 that stored in it, get a breakdown of variable copy triples
 of the form:
type, to name, from name

in the right format for use in the below generate_copy_assignment
function.  *)
let rec get_copy_types_for typemap typ to_basename from_basename =
    (* Note that this assumes that the types are the same.  *)
    (* I would like to reuse the expand_types in gir_topology,
    but I think this is better with it's own, as it's trying
    to expand the types in a particular way to match tbe
    generate_copy_assignment function.  *)
    let rec subtyps_of n prepend_names_1 prepend_names_2 =
        match n with
        | Array(sub_type, dim) ->
                let stys =
                    (* Clear both the basenames when
                    recursing from arrays --- the aim is to
                    generate an array that is like
                    [ ['basename'],
                      []
                      ['postfix']
                      ]
                      etc.  *)
                    get_copy_types_for typemap sub_type [] [] in
                (* Add this array to the head of all the things.  *)
                List.map stys ~f:(fun (s, names_1, names_2)->
                    Array(s, dim),
                    prepend_names_1 :: names_1,
                    prepend_names_2 :: names_2
                )
        | Struct(sname) ->
                let meta = Hashtbl.find_exn typemap.classmap sname in
                let submap = get_class_typemap meta in
                let members = get_class_fields meta in
                let subtypemap = { typemap with variable_map = submap } in
                let subvars = List.concat (List.map members ~f:(fun m ->
                    let mtyp = Hashtbl.find_exn subtypemap.variable_map m in
                    subtyps_of mtyp [m] [m])) in
                List.map subvars ~f:(fun (sty, names_1, names_2) ->
                    let new_names_1 = match names_1 with
                    | x :: xs -> (prepend_names_1 @ x) :: xs
                    | _ -> assert false (* Think this shouldn't be possible because the subtypes will have to have been expanded? *)
                    in
                    let new_names_2 = match names_2 with
                    | x :: xs -> (prepend_names_2 @ x) :: xs
                    | _ -> assert false
                    in
                    (sty,
                    new_names_1,
                    new_names_2)
                )
        | Pointer(styp) ->
                (* Treating pointers as transparent here is the right idea? *)
                let subcalls = get_copy_types_for typemap styp to_basename from_basename in
                List.map subcalls ~f:(fun (t, name_1, name_2) ->
                    (Pointer(t), name_1, name_2)
                )
        | typ ->
                [(typ, [prepend_names_1], [prepend_names_2])]
    in
    subtyps_of typ to_basename from_basename

let rec generate_variable_reference_to namerefs =
    match namerefs with
    | AnonymousName -> raise (GenerateGIRException "Can't genereate a variable reference from anonymous names!")
    (* Name mappings are easy. *)
    | Name(n) -> Variable(Name(n))
    | StructName(nlist) ->
            match nlist with
            | [] -> raise (GenerateGIRException "Can't generate variable references from empty names!")
            | [Name(x)] -> Variable(Name(x))
            | [x] -> raise (GenerateGIRException "Pretty sure this sin't possible")
            | xs ->
                    (* This could probably be more efficient. *)
                    let xs = List.rev xs in
                    let head_xs, tail_xs = List.hd_exn xs, List.tl_exn xs in
                    let head_name = (match head_xs with
                    | Name(x) -> x
                    | _ -> raise (GenerateGIRException "pretty sure this isn't possible")
                    ) in
                    let original_tail = List.rev tail_xs in
                    MemberReference(generate_variable_reference_to (StructName(original_tail)), Name(head_name))

let generate_const_reference_to const =
	Constant(const)

let generate_dim_reference typelookup dim =
	match dim with
	| DimVariable(name, mode) -> (
			(* Generate a loop for each of the dimvars.  *)
			(* Also try just a straight up assignment.  *)
			let girname = VariableReference(Variable(Name(name_reference_to_string name))) in
			girname
	)
	| DimConstant(from) ->
			let name = VariableReference(generate_const_reference_to (Int64V(from))) in
			name

(*  This should generate a list of functions
that can be used to generate wrappers when
given a simple assignment sequence Assignment.  *)
(* It also keeps track of the index variables *)
let rec generate_loop_wrappers_from_single_dimension typelookup constraints =
    (* let () = Printf.printf "Generating loop wrappers from dimension %s\n" (dimension_constraint_to_string constraints) in *)
	(* Get the dimension out of the constraints *)
	let dim = match constraints with
	| DimensionConstraints(cons, dim) -> dim
	in
	match dim with
	(* Does this need to throw?  Could we get away w/out throwing
and putting a TODO note like in the backend? *)
	| EmptyDimension -> raise (GenerateGIRException "Dimensions should be assigned by generate gir")
	| SingleDimension(dim) ->
			let indvar = new_induction_variable () in
			let maxref = generate_dim_reference typelookup dim in
			let in_loop_assign = (fun assign ->
				LoopOver(assign, indvar, maxref)
				)
			in
			(in_loop_assign, [indvar])
	| MultiDimension(dims, opmode) ->
			let dimnames = List.map dims ~f:(fun dim ->
				generate_dim_reference typelookup dim
			) in
			(* let () = Printf.printf "From dims %s, generated name references %s\n" (dimension_value_list_to_string dims) (expression_list_to_string dimnames)  in *)
			let maxvar = new_variable () in
			let _ = Hashtbl.add typelookup ~key:(gir_name_to_string maxvar) ~data:(Int64) in
			let indvar = new_induction_variable () in

			(* The multiplication function used here takes
			   two args, so use this recursion to generate
			   a sequence of assigns to get the assignment to
			   maxvar with the real max.  *)
			let precode = (
			match opmode with
			(* Note I'm pretty sure that new modes can reuse
			the code below almost entirely.  *)
			| DimMultiply ->
				let rec generate_precode maxvar vs =
					match vs with
					| [] -> assert false (*think this sin't possible? *)
					| [v] -> [Assignment(LVariable(maxvar), Expression(v))]
					| v :: vs ->
							(* Generate the sub multiplications *)
							let new_maxvar = new_variable() in
							let new_tempvar = new_variable() in
							let _ = Hashtbl.add typelookup ~key:(gir_name_to_string new_maxvar) ~data:(Int64) in
							let _ = Hashtbl.add typelookup ~key:(gir_name_to_string new_tempvar) ~data:(Int64) in
							let sub_precode = generate_precode (Variable(new_maxvar)) vs in

							(* Add def for new_maxvar *)
							[Definition(new_maxvar, false, Some(Int64), None);
							Definition(new_tempvar, false, Some(Int64), None);
							Assignment(LVariable(Variable(new_tempvar)), Expression(v))] @
							sub_precode @ [
								(* Do multiply with V and then assign
								   to the maxvar passed in.  *)
								Assignment(LVariable(maxvar),
									Expression(FunctionCall(
										FunctionRef(Name("Multiply")),
										VariableList([Variable(new_maxvar); Variable(new_tempvar)])
									))
								)
							]
				in
				generate_precode (Variable(maxvar)) dimnames
			) in
			let result = (fun assign ->
				Sequence(
					[Definition(maxvar, false, Some(Int64), None)] @
					precode @ [
					LoopOver(assign, indvar, VariableReference(Variable(maxvar)))
				])
			) in
			(result, [indvar])

(* Given a multi-dimensional set of constraints, generate
   an assign function for that loop. *)
let rec generate_loop_wrappers_from_dimensions typelookup constraints =
    match constraints with
    | [] -> ((fun assign -> assign), [])
    | x :: xs ->
            (* Outer one first. *)
            let outer_assfunc, outer_indvars =
                generate_loop_wrappers_from_single_dimension typelookup x
            in
            let inner_assfunc, inner_indvars =
                generate_loop_wrappers_from_dimensions typelookup xs
            in
            (fun assign ->
                outer_assfunc (inner_assfunc assign)),
               outer_indvars @ inner_indvars

let rec maybe_create_reference_from post_indexes indvarnames =
	(* let post_indexes_str =
		String.concat ~sep:", " (List.map post_indexes ~f:(fun p ->
			match p with
			| None -> "None"
			| Some(p) -> variable_reference_to_string p
		)
		) in
	let indvarnames_str = gir_name_list_to_string indvarnames in
	let () = Printf.printf "Looking at names %s and %s\n" (post_indexes_str) (indvarnames_str) in *)
	match post_indexes, indvarnames with
	| [], [] -> raise (GenerateGIRException "Must be non empty!")
	| p :: [], [] -> p
	| p :: ps, i :: is ->
			let subref = maybe_create_reference_from ps is in (
			match p, subref with
			| None, None ->
					None
			| None, Some(subref) ->
					Some(IndexReference(subref, VariableReference(Variable(i))))
			| Some(p), None ->
					Some(IndexReference(
						p, VariableReference(Variable(i))
					))
			| Some(p), Some(subref) ->
					Some(
						build_reference_chain
							(IndexReference(
								subref,
								VariableReference(Variable(i))
							))
							p
					)
			)
	| _ -> raise (GenerateGIRException "Indexes and indvarnames must be the same length!")

let create_reference_from post_indexes indvarnames =
	match post_indexes with
	| AssignVariable(post_indexes) ->
			(* This thing only works with the list backwards apparently.  *)
			let result = maybe_create_reference_from (List.rev (generate_gir_names_for post_indexes)) indvarnames in
			(
			match result with
			| None -> raise (GenerateGIRException "Ended up producing no result!")
			| Some(x) -> x
			)
	| AssignConstant(c) ->
			Constant(c)

(* This generates a set of functions that take
a list representing the variables that are used to index
this.  *)
let generate_assign_functions conversion_function_name fvar_index_nestings tvar_index_nesting =
	if (List.length fvar_index_nestings) = 0 then
		[]
	else
		List.map fvar_index_nestings ~f:(fun fvar_ind_nest -> (
				fun index_vars ->
					(* We expect one index_var for each fromvar_index and each tovar_index --- those
					capture the parts of the variable names
					that are refered to by each.  *)
					(* let () = Printf.printf "Ind nest is %s\n" (assignment_type_to_string fvar_ind_nest) in
					let () = Printf.printf "Index vars is %s\n" (gir_name_list_to_string index_vars) in
					let () = Printf.printf "tvars is %s\n" (name_reference_list_to_string tvar_index_nesting) in
					let () = Printf.printf "fvars is %s\n" (assignment_type_list_to_string fvar_index_nestings) in
                    let () = Printf.printf "assignment is %s\n" (assignment_type_to_string fvar_ind_nest) in
					let () = Printf.printf "fvar ind nest length is %d \n " (List.length fvar_index_nestings) in *)
					let () = assert(match fvar_ind_nest with
					| AssignVariable(vlist) -> List.length(vlist) - 1 = List.length index_vars
					| AssignConstant(c) -> true) in
					let () = assert(List.length(tvar_index_nesting) - 1 = List.length index_vars) in
					(* Get the LVars --- if there
					are no indexes then it is just
					a list, otherwise we need to do 
					a fold. *)
                    let conversion_ref = FunctionRef(conversion_function_name) in
					let lvars = LVariable(create_reference_from (AssignVariable(tvar_index_nesting)) index_vars) in
                    let rvars: rvalue = Expression(FunctionCall(conversion_ref, VariableList([create_reference_from fvar_ind_nest index_vars]))) in
					Assignment(lvars, rvars)
			)
		)

let get_define_for options typemap definition_type define_internal_before_assign escaping_vars vnameref =
    let escapes = List.mem escaping_vars (variable_reference_to_string vnameref) ~equal:Utils.string_equal in
	let () = if options.debug_generate_gir then
        let () = Printf.printf "Looking for definition type of %s, have esjjcapeing vars %s\n" (variable_reference_to_string vnameref) (String.concat ~sep:", " escaping_vars) in
        Printf.printf "Getting define for type%s\n" (Option.value (Option.map definition_type ~f:synth_type_to_string) ~default:"None")
	else ()
	in
	let result = if (define_internal_before_assign || escapes) then
		match vnameref with
		| Variable(nam) ->
                (* Note that this might not be the same as the definition type --- it might e.g. be
                an int member of a struct, making htis a struct.  *)
                let top_def_type = definition_type in
                Definition(nam, escapes, top_def_type, None)
        | Constant(c) ->
                (* Constants shouldn't need a def type?  And also probably shouldn't be being generated
                here? *)
            EmptyGIR
        (* More complicated types shouldn't reach here --- get_define_for should only be called
        with the top-level types.  The aim is for the backend to expand out the definition
        into whatever complex shit is required for that definition, e.g. with a struct { int *name }
        setup.  *)
		| _ -> raise (GenerateGIRException "Don't know how to define a more complicated type right now!")
	else
		(* Don't want to define anything that doesn't escape and has already been defined! *)
		EmptyGIR
	in
	let () = if options.debug_generate_gir then
		Printf.printf "Got result %s\n" (gir_to_string result)
	else ()
	in
	result

(* TODO -- really need to fix this crappy conversion
from the name_references to gir names --- there's way
too many exceptions flying around in all these implementations
*)
(* takes some variable that's been split up into e.g.
cpx ['x', 'y'] where it's intended use is
cpx.x[i].y[j] and turns it into cpx.x.y.  It's used
in tandem with the above function, not sure whether
it's actually going to be useful for e.g. multiple
dimensions or not.  *)
let rec define_name_of index_points =
	match index_points with
	| [] -> raise (GenerateGIRException "Can't define empty varaiable")
	(* Do the define before the first index? idk *)
	| Some(x) :: xs ->
			get_top_gir_name x
	(* Pretty sure it should always hit ^^^ *)
	| None :: xs -> define_name_of xs

(* Either empty or singleton --- used to concat elsewhere *)
let get_unwarpped_dim_dependency (dimension_value): gir_name list =
    match dimension_value with
    | DimVariable(vnam, relation) -> (match vnam with
        | Name(n) -> [Name(n)]
        | _ -> raise (GenerateGIRException "Can't convert anything that isn't a name!")
        )
	(* | DimMultipleVariables(vs, op) ->
			List.map vs ~f:(fun v -> match v with
				| Name(n) -> Name(n)
				| _ -> raise (GenerateGIRException "Can't convert anything that isn't a name")
			)
	*)
	| DimConstant(c) -> []

let generate_conversion_function conv = match conv with
    | IdentityConversion ->
            (* We don't need to do anything here --- this
            should be eliminated by further passes.
            *)
            EmptyGIR, Name("identity")
	| PowerOfTwoConversion ->
			EmptyGIR, Name("Pow2")
	| DivideByConversion(mby) ->
			(* OK, so really this shouldn't need to be a customizeable
			function, but it fits easier with the rest of the
			passes... *)
			let fname = Name("DivideByX_" ^ (gir_name_to_string (new_conversion_function ()))) in
			let argname = new_variable() in
			let returnvar = new_variable() in
			let typelookup = Hashtbl.create (module String) in
			(* TODO --- We should be smarter about the types. *)
			let _ = Hashtbl.add typelookup ~key:(gir_name_to_string argname) ~data:(Int64) in
			let _ = Hashtbl.add typelookup ~key:(gir_name_to_string returnvar) ~data:(Int64) in
			let _ = Hashtbl.add typelookup ~key:(gir_name_to_string fname) ~data:(Fun([Int64], Int64)) in
			let functiondef = FunctionDef(fname, [argname],
				Sequence([
					Definition(returnvar, true, Some(Int64), None);
					Assignment(
						LVariable(Variable(returnvar)),
						Expression(FunctionCall(FunctionRef(Name("IntDivide")),
						VariableList([
							Variable(argname); Constant(Int64V(mby))
						])))
					);
					Return(VariableReference(Variable(returnvar)))
				]),
				typelookup
			) in
			functiondef, fname
    | Map(ftype, ttype, to_from_list) ->
			let to_from_list_synths = List.map to_from_list ~f:(fun (tov, fromv) ->
				(range_value_to_synth_value tov, 
				 range_value_to_synth_value fromv)
			) in
            let fname = new_conversion_function () in
            let argname = new_variable () in
            let returnvar = new_variable () in
            (* Create the typelookup.  *)
            let typelookup = Hashtbl.create (module String) in
            (* And add the functions to it.  *)
            let _ = Hashtbl.add typelookup ~key:(gir_name_to_string argname) ~data:ftype in
            let _ = Hashtbl.add typelookup ~key:(gir_name_to_string returnvar) ~data:ttype in
            let _ = Hashtbl.add typelookup ~key:(gir_name_to_string fname) ~data:(Fun([ftype], ttype)) in
            FunctionDef(fname, [argname],
                Sequence([
					Definition(returnvar, true, Some(ttype), None);
					Assignment(
						LVariable(Variable(returnvar)),
						Expression(GIRMap(argname, to_from_list_synths))
					);
					Return(VariableReference(Variable(returnvar)))
                ]
                ),
                typelookup
            ), fname

let get_definition_type_for options escapes validmap typemap v =
	match options.compile_settings.allocation_mode with
    | StackAllocationMode -> (* Variable length supported. *)
            Some(type_of_name_reference typemap v)
    | HeapAllocationMode -> (* Also have variable length.  *)
            Some(type_of_name_reference typemap v)
    | StaticAllocationMode ->
			if escapes then
				(* Right now, we still heap allocated
				escaping variables.  Need to fix the skeleton
				range map generator to change this
				(as user variables need to have range
				maxes to use this).   See
                skeleton_range_checker/generate_post_check_ranges
                which needs to be expanded to include the user types.
                *)
				Some(type_of_name_reference typemap v)
			else
            (* We need to allocate the biggest
            array that might have been used.  *)
            let rec size_concreteization ty =
                match ty with
                | Array(sb, dim) ->
                        let stype_new = size_concreteization sb in
						let dimmax_from_single_dimension d =
							match d with
                        | DimConstant(c) -> Some((c))
                        | DimVariable(dimv, relation) ->
                                let range = Hashtbl.find validmap (name_reference_to_string dimv) in
								let raw_max = (
                                match range with
                                | None ->
										(* This should really be the below exception, but
										we sometimes generate defines
										for things that shouldn't have defines.
										This is the next-best way to do this I suppose.  *)
										None
										(* let () = Printf.printf "For array %s with dim variable %s could not find ranges\n" (v) (name_reference_to_string dimv) in *)
										(* raise (GenerateGIRException "Error: when in static allocation mode, array parameters must have length restrictions (try specifying in accelerator file") *)
                                | Some(r) ->
                                        match range_max r with
                                        | RangeInteger(i) -> Some(i)
                                        | _ -> assert false (* Can't have non-integer array length *)
								) in
								(
								match relation with
								| DimEqualityRelation -> Option.map raw_max ~f:(fun r -> (r))
								| DimPo2Relation -> Option.map raw_max ~f:(fun r -> (Utils.power_of_two r))
								| DimDivByRelation(mby) ->
										Option.map raw_max ~f:(fun r -> (r / mby))
								)
						in
						let dimmax = match dim with
						| SingleDimension(d) ->
								Option.map (dimmax_from_single_dimension d) ~f:(fun d -> DimConstant(d))
						| MultiDimension(ds, op) ->
								let rmaxes: int option list = List.map ds ~f:dimmax_from_single_dimension in
								let int_result = (
								match op with
								| DimMultiply ->
									List.fold ~init:(Some(1)) ~f:(fun v1 -> (fun v2 -> (match v1, v2 with
									| Some(i1), Some(i2) -> Some(i1 * i2)
									| _, _ -> None
									))) rmaxes
								) in
								Option.map int_result ~f:(fun r -> DimConstant(r))
                        | EmptyDimension -> assert false
						in
                        Option.join (Option.map dimmax ~f:(fun m -> Option.map stype_new ~f:(fun s -> Array(s, SingleDimension(m)))))
                | Pointer(sp) ->
						Option.map (size_concreteization sp) ~f:(fun p -> Pointer(p))
                | other -> Some(other)
                (* Perhaps we should handle structs specially here.  *)
                (* Note: I don't think that we will have to, since
                 those don't have variable length. *)
            in
            size_concreteization (type_of_name_reference typemap v)

(* Definitions should be for the outer-most variable I think. 
 (e.g. if we have a complex[].real, we just need to define
 the complex[] part.  *)
let get_definition_type_for_tovar options validmap typemap escaping_variables tovars =
	match tovars with
	| [] -> raise (GenerateGIRException "Defining empty variable")
	| x :: xs ->
            let escapes = List.mem escaping_variables (name_reference_to_string x) ~equal:Utils.string_equal in
			get_definition_type_for options escapes validmap typemap x

let generate_variable_reference_to_dimension dim =
	match dim with
		| DimVariable(nr, DimEqualityRelation) -> generate_variable_reference_to nr
		| DimConstant(c) -> Constant(Int64V(c))
		| _ -> assert false (* TODO -- implement *)

(* Build some loops that can do an N-dimensional copy.  *)
(* This returns two things: a function, from (GIR -> GIR)
that gives you the loop (the function takes the body
of the innermost *)
(* and a list of indexes.  *)
let rec generate_gir_copy_loop typemap copytype =
    match copytype with
        | Array(subty, dims) ->
                let sub_body, sub_indvars =
                    generate_gir_copy_loop typemap subty
                in
                (* Generate the loop wrappers from the dims *)
                let dim =
                    match dims with
                    (* Only support equality relation here, although we could of course support more.  *)
                    | SingleDimension(dm) -> VariableReference(generate_variable_reference_to_dimension dm)
                    | MultiDimension(nrs, op) ->
                            let fname = match op with
                            | DimMultiply -> "PRODUCT"
                            in
							(* TODO -- support multiple prods with
							a fold.  *)
                            FunctionCall(FunctionRef(Name(fname)), VariableList(List.map nrs ~f:(fun n -> generate_variable_reference_to_dimension n)))
                    | EmptyDimension -> assert false (* Shouldnae happen? *)
                in
                (* Compute the index variable for this loop.  *)
                let indvar = new_induction_variable () in
                let result = (fun assignment -> LoopOver(sub_body assignment, indvar, dim)) in
                result, indvar :: sub_indvars
        | Struct(sname) ->
                assert false (* This should not be handled here,
                but rather split-up prior to this phase.  The
                individual raw types should be handled.  *)
        | Pointer(sty) ->
                let sub_assignment, sub_indvars =
                    generate_gir_copy_loop typemap sty
                in
                sub_assignment, sub_indvars (* Think there is nothing to do? *)
        | _ -> (* Everything else can just be a raw assign? *)
                (fun ass -> ass), []

(* Returns lvalue, rvalue, which can then easily be piped
   into an assignment.  *)
(* This is an iterative approach *)
let rec generate_copy_assignment_option typ to_reference from_reference tovars fromvars indvars: (variable_reference option * variable_reference option) =
    (* So there is a generic alsogirhm for this,
    except in the case of pointers, where we need
    the type information.  *)
    (* In any-case, this type-specific version doesn't
    expand anything.  *)
	(* let () = Printf.printf "Generating copy for type %s\n" (synth_type_to_string typ) in
	let () = Printf.printf "Refernces are: to (%s) from (%s)\n" (variable_reference_option_to_string to_reference) (variable_reference_option_to_string from_reference) in *)
    let to_result, from_result = match typ with
    | Array(sty, dim) ->
            (
            match indvars with
            | i :: is ->
                    let fromname, fromrest = match fromvars with
					| x :: xs -> x, xs
					| _ -> assert false (*Shouldn't be possible?  Or is this what happens with annon refs? *)
					in
					let toname, torest = match tovars with
					| x :: xs -> x, xs
					| _ -> assert false (*Shouldn't be possible?  Or is this what happens with annon refs? *)
					in
                    (* let () = Printf.printf ("Fromname is %s, toname is %s") (gir_name_list_to_string fromname) (gir_name_list_to_string toname) in *)
					let this_indref = VariableReference(Variable(i)) in
					(* Build the next level of assignment. *)

                    (* The subrefs are the end of the expression,
                       and the refs are the head of the expression.
                       This is in the opposite direction to what
                       you would expect, so we have to do a full
                       rebuild of the reference.  *)
                    let to_basename = match to_reference with
                    | Some(t) ->
                            (
                            match build_reference_chain_optional (Some(t)) (build_reference_chain_from_list toname) with
                            | Some(nr) -> nr
                            | None -> assert false (* as below? *)
                            )
                    | None ->
                            match build_reference_chain_from_list toname with
                            | Some(nr) -> nr
                            | None -> assert false (* I think this can't be true in a valid type? *)
                    in
                    let new_toref = IndexReference(to_basename, this_indref) in
                    let from_basename = match from_reference with
                    | Some(t) ->
                            (
                            match build_reference_chain_optional (Some(t)) (build_reference_chain_from_list fromname) with
                            | Some(nr) -> nr
                            | None -> assert false
                            )
                    | None ->
                            match build_reference_chain_from_list fromname with
                            | Some(nr) -> nr
                            | None -> assert false
                    in
                    let new_fromref = IndexReference(from_basename, this_indref) in
                    (* Generate the copy for the rest of the type.  *)
                    let lvar_subref, rvar_subref = generate_copy_assignment_option sty (Some(new_toref)) (Some(new_fromref)) torest fromrest indvars in
                    lvar_subref, rvar_subref
            | _ -> assert false
            )
    | Struct(n) ->
            (* Think that these should be pre-filtered.
            I think the code below does actually work, but this
            is a useful debug trigger IMO.  *)
            let () = assert false in
            (* In this function, we transparently pass through
               structs.  It helps spread the complexity out
               a bit --- the breaking down of a single copy
               into multiple copies is handled by
               get_copy_types_for.  *)
            let torest = match tovars with
            | x :: y :: ys -> (x @ y) :: ys
            | _ -> assert false
            (* A struct must not be the bottom
            element in the copy?  i.e. must be expanded into
            sub elements.  *)
            in
            let fromrest = match fromvars with
            | x :: y :: ys -> (x @ y) :: ys
            | _ -> assert false
            in
            (* The typ is passed unchanged because structs
               are 'invisible' to this function, in the same
               way that pointers usually are.  *)
            generate_copy_assignment_option typ to_reference from_reference torest fromrest indvars
    | Pointer(sty) -> assert false (* unsupported --- need the deref thing I think? *) (* Or maybe it can be transparently supported? *)
    | other ->
            let toref = match tovars with
            | [xs] -> build_reference_chain_optional to_reference (build_reference_chain_from_list xs)
            | _ -> assert false
            in
            let fromref = match fromvars with
            | [xs] -> build_reference_chain_optional from_reference (build_reference_chain_from_list xs)
            | _ -> assert false
            in
            toref, fromref
			(* Is there a valid case where this is not? *)
    in
    (* let () = Printf.printf "For type %s\n" (synth_type_to_string typ) in
    let () = Printf.printf "Assignment result is %s\n" (variable_reference_option_to_string to_result) in *)
    to_result, from_result

let generate_copy_assignment typ tovars fromvars indvars =
    (* let () = Printf.printf "For type %s, tovars are %s\n" (synth_type_to_string typ) (gir_name_list_list_to_string tovars) in *)
    let l, r = generate_copy_assignment_option typ None None tovars fromvars indvars in
    match l, r with
    | Some(l), Some(r) -> l, r
    | _ -> assert false (* No full type assignment should be empty? *)

(* Given a fromvar, a tovar, and two types, generate a 
   copy statement between them. *)
(* Note that they must both have the same type to copy
   blindly between them.  *)
let rec generate_gir_copy typemap fromvars tovars copytype =
    (* Get the loops *)
    let loop, indvars = generate_gir_copy_loop typemap copytype in
    (* Get the innermost assignment *)
    let lvalue, rvalue = generate_copy_assignment copytype tovars fromvars indvars in
	let assignment = Assignment(LVariable(lvalue), Expression(VariableReference(rvalue))) in
    (* Build the loop with this assignment.  *)
    loop assignment

(* Generate a list of copies for a non-trivial type.  *)
let rec generate_gir_copies typemap tovars fromvars copytype =
    let expanded_types = get_copy_types_for typemap copytype tovars fromvars in
    List.map expanded_types
        ~f:(fun (typ, basenames_to, basenames_from) ->
            generate_gir_copy typemap (to_gir_name_list_list basenames_from) (to_gir_name_list_list basenames_to) typ
        )

let generate_gir_for_binding (apispec: apispec) (iospec: iospec) typemap define_internal_before_assign insert_return (options: options) validmap (skeleton: flat_skeleton_binding) =
	(* First, compute the expression options for each
	   binding, e.g. it may be that we could do
	   x = cos(y) or x = sin(y) or x = y. 
	   Note that I think this should no longer happen
	   here, but I'm going to leave that comment anyway. *)
    (* Escaping variables must be defined differently in C-like
    targets.  *)
    let escaping_variables = Utils.set_difference Utils.string_equal iospec.returnvar iospec.funargs in
    (* Not sure this is right -- it should also include
    the return value when that feature is added.  *)
	let unescaping_variables = apispec.funargs in
	let expression_options, required_fun_defs = List.unzip (List.map skeleton.flat_bindings ~f:(fun (single_variable_binding: flat_single_variable_binding) ->
		(* There may be more than one valid dimension value.
		   generate assignments based on all the dimension values. *)
        (* TODO --- fix this shit -- I'm pretty sure
        the loop gen is broken for 2D loops.  *)
		let () = if options.debug_generate_gir then
			let () = Printf.printf "Starting new binding gen for binding\n" in
			let () = Printf.printf "%s\n" (flat_single_variable_binding_to_string single_variable_binding) in
			let () = Printf.printf "(END BINDING)\n" in
            ()
		else () in
        let loop_wrappers =
            generate_loop_wrappers_from_dimensions typemap.variable_map single_variable_binding.dimensions in
		let conversion_function, conversion_function_name = generate_conversion_function single_variable_binding.conversion_function in
        let fvars_indexes = single_variable_binding.fromvars_index_nesting in
        let tovar_indexes = single_variable_binding.tovar_index_nesting in
        (* Convert the variable references mentioned
            in the bindings into real variable refs.  *)
		(* Generate the possible assignments *)
		let assign_funcs = generate_assign_functions conversion_function_name fvars_indexes tovar_indexes in
		let definition_type = get_definition_type_for_tovar options validmap typemap escaping_variables tovar_indexes in
		(* Get the define if required.  *)
		let define =
			let () = if options.debug_generate_gir then
				let () = Printf.printf "Have the following tovars for the generation round:\n " in
				let () = Printf.printf "%s\n" (name_reference_list_to_string single_variable_binding.tovar_index_nesting) in
				let () = Printf.printf "Considering the following escaping vars %s\n" (String.concat ~sep:", " escaping_variables) in
			() else () in
			(* This returns an empty GIR if the define shouldn't be made
			(e.g. this is a post code and doesn't escape.)  *)
			get_define_for options typemap definition_type define_internal_before_assign escaping_variables (define_name_of (generate_gir_names_for tovar_indexes))
		in
        let () =
            if options.debug_generate_gir then
                let () = Printf.printf "------\n\nFor variable %s\n" (flat_single_variable_binding_to_string single_variable_binding) in
				let () = Printf.printf "Valid dimensions were %s\n" (dimension_constraint_list_to_string single_variable_binding.dimensions) in
				let () = Printf.printf "Loop assignment functions are %d\n" (List.length assign_funcs) in
				Printf.printf "Define used is %s\n" (gir_to_string define)
            else
                () in
		(* Do every combination of assignment loops and assign funcs. *)
		let assignment_statements =
            match loop_wrappers with
            | lwrap, indvars ->
                    List.map assign_funcs ~f:(fun assfunc ->
                        lwrap (assfunc indvars))
        in
		let assigns_with_defines =
			if (List.length assignment_statements) > 0 then
				List.map assignment_statements ~f:(fun ass -> Sequence([define; ass]))
			else
				(* Some vars can be define-only *)
				[define]
		in
		(* Also return the conversion functions needed for this
			assign. *)
		assigns_with_defines, conversion_function
	)
	)in
	(* We now have a expression list list, where we need one element
	   from each sublist in sequence to form complete assignment
	   tree.  *)
	let () = if options.debug_generate_gir then
		let () = Printf.printf "Have the following expression options before cross product: %s\n"
			(gir_list_list_to_string expression_options) in
		let () = Printf.printf "This amounts to %d lists\n" (List.length expression_options) in
		()
	else () in
	let expr_lists: gir list list = cross_product expression_options in
	(* Now we have a expression list list where each set
	   is a full set of assignments.  Convert each expr list
	   to a sequence.  *)
	let returnstatement =
		if insert_return then
			let frees =
				(* Don't free everything --- only things that
				don't escape.  *)
                List.map unescaping_variables ~f:(fun v ->
                    Free(Variable(Name(v)))
                )
			in
			match iospec.returnvar with
			| [] -> Sequence(frees)
			| [x] ->
                    Sequence(
                        frees @
						[Return(VariableReference(Variable(Name(x))))]
                    )
			| _ ->
					(* We should really suppor this at this point -- but since C/C++ is the
					only supported backend right now, we dont really need
					to yet. *)
					raise (GenerateGIRException "Multireturn functions not currently supported")
		else
			EmptyGIR
	in
	let code_options = List.map expr_lists ~f:(fun exprs -> Sequence(exprs @ [returnstatement])) in
	(* Do a quick cleanup --- e.g. making sure that there are no double
	defines, which this approach is prone to generating.  *)
	let cleaned_code_options = List.map code_options ~f:gir_double_define_clean in
	cleaned_code_options, required_fun_defs

let rec all_dimvars_from dimtype =
	match dimtype with
			| EmptyDimension -> []
			| SingleDimension(nms) -> get_unwarpped_dim_dependency nms
			| MultiDimension(nms, op) ->
					List.concat (List.map nms ~f:get_unwarpped_dim_dependency)

let rec type_topo_dependencies (nam, typ) =
	match typ with
	| Array(subtyp, dimtype) ->
			let _, subdeps = type_topo_dependencies (nam, subtyp) in
			(* let () = Printf.printf "For name %s have deps %s \n " (name_reference_to_string nam) (String.concat (List.map (all_dimvars_from dimtype) ~f:name_reference_to_string )) in *)
			(nam, (all_dimvars_from dimtype) @ (subdeps))
	| Pointer(sty) ->
			type_topo_dependencies (nam, sty)
	(* All non-array types are not dependent types
		in the languages we are currently supporting.  *)
	| _ -> (nam, [])

(* Do a topological sort on a list of (name, depname) dependencies *)
(* This is a horribly inefficient toposort, but I expect
the input problem size to be quite small so shouldn't be an
issue.  *)
let rec member x ys =
	match x, ys with
	| _, [] -> false
	| Name(nm), (Name(y) :: ys) -> ((String.compare nm y) = 0) || (member x ys)
	(* | _ -> raise (GenerateGIRException "Unimplemented") *)

let rec toposort_search_for_deps name possdeps =
	match possdeps with
	| [] -> [], []
	| (nm, deps) :: rest -> 
			let subdeps, nondeps = toposort_search_for_deps name rest in
			if member name deps then
				((nm, deps) :: subdeps), nondeps
			else
				subdeps, ((nm, deps) :: nondeps)

let rec toposort names =
	match names with
	| [] -> []
	| ((nm, deps)) :: rest ->
			let afters, befores = toposort_search_for_deps nm rest in
			(toposort befores) @ (nm :: (toposort afters))


let generate_define_statemetns_for options validmap typemap (iospec: iospec) api =
	(* Need to make sure that types that are dependent on each
	   other are presented in the right order.  *)
	(* Compute any defines that are needed for the returnvars. *)
	let unpassed_returnvars = Utils.set_difference Utils.string_equal iospec.returnvar iospec.funargs in
	let names = List.map (api.livein @ unpassed_returnvars) ~f:(fun n -> (Name(n), Hashtbl.find_exn typemap.variable_map n)) in
	let sorted_names = toposort (List.map names ~f:type_topo_dependencies) in
	let () = if options.debug_gir_generate_define_statements then
		let () = Printf.printf "Names %s\n" (String.concat((List.map names ~f:(fun (n, s) -> (match n with Name(x) -> x) ^ (synth_type_to_string s))))) in
		let () = Printf.printf "Sorte names %s\n" (String.concat ~sep:", " (List.map sorted_names ~f:gir_name_to_string)) in
		let () = Printf.printf "Livein is %s\n" (String.concat ~sep:", " iospec.livein) in
		let () = Printf.printf "Unpassed return vars are %s\n" (String.concat ~sep:", " unpassed_returnvars) in
		()
	else
		()
	in
	let typed_sorted_names = List.map sorted_names ~f:(fun n ->
        let escapes = List.mem unpassed_returnvars (gir_name_to_string n) ~equal:Utils.string_equal in
		(n, get_definition_type_for options escapes validmap typemap (gir_name_to_name_reference n))
	) in
    (* Generate a define for each input variable in the API *)
	List.map typed_sorted_names ~f:(fun (x, xtyp) ->
		if List.mem unpassed_returnvars (gir_name_to_string x) ~equal:Utils.string_equal then
			Definition(x, true, xtyp, None)
		else
			Definition(x, false, xtyp, None)
	)

let generate_gir_for options apispec iospec (skeleton: skeleton_pairs) =
	let () = if options.debug_generate_gir then
		let () = Printf.printf "Starting generation for new skeleton pair\n" in
		let () = Printf.printf "Pair is\n %s \n" (skeleton_pairs_to_string skeleton) in
		()
	else () in
    (* Get the define statements required for the API inputs.  *)
	(* Define the variables before assign in the pre-skeleton case.  *)
	let pre_gir, pre_required_fun_defs = generate_gir_for_binding apispec iospec skeleton.typemap true false options skeleton.post_check_validmap skeleton.pre in
	let post_gir, post_required_fun_defs = generate_gir_for_binding apispec iospec skeleton.typemap false true options skeleton.post_check_validmap skeleton.post in
    (* Keep track of the variable length assignments that have been made. *)
	let all_fundefs = pre_required_fun_defs @ post_required_fun_defs in
	let res = List.cartesian_product pre_gir post_gir in
	let () = if options.debug_generate_gir then
		let () = Printf.printf "Finished generation of candidata pre programs.  Program are:\n%s\n"
			(String.concat ~sep:"\n\n" (List.map pre_gir ~f:gir_to_string)) in
		let () = Printf.printf "Finsihed generation of candiates post programs. Programs are:\n%s\n"
			(String.concat ~sep:"\n\n" (List.map post_gir ~f:gir_to_string)) in
		Printf.printf "Found %d pre and %d post elements\n" (List.length pre_gir) (List.length post_gir)
	else () in
    List.map res ~f:(fun (pre, post) ->
		let new_typemap =
			{ skeleton.typemap with variable_map = clone_variablemap skeleton.typemap.variable_map }
		in
		(skeleton, pre, post, new_typemap, all_fundefs, skeleton.rangecheck, skeleton.inputmap))


let generate_gir (options:options) iospec api skeletons: ((gir_pair) list) =
	let result = List.concat ((List.map skeletons ~f:(fun skel ->
		generate_gir_for options api iospec skel))) in
	let () = if options.dump_generate_gir then
		let () = Printf.printf "Generated %d GIR-pair programs\n" (List.length result) in
		Printf.printf "Printing these programs below:\n%s\n" (String.concat ~sep:"\n\n\n" (List.map result ~f:(fun(orig_skel, pre, post, typemap, funs, range, map) ->
			"Pre:" ^ (gir_to_string pre) ^ "\nPost: " ^ (gir_to_string post))))
	else () in
	List.map result ~f:(fun (original_skeleton, pre, post, typemap, fundefs, range_checker, inputmap) ->
		{
			original_pairs = original_skeleton;
			pre = pre;
            post = post;
			typemap = typemap;
			fundefs = fundefs;
			inputmap = inputmap;
			range_checker = Option.map range_checker ~f:(fun checker ->
            {
                    condition = checker;
            });
		})
