open Core_kernel;;
open Spec_definition;;
open Spec_utils;;
open Gir_utils;;
open Options;;
open Gir;;

exception CXXGenerationException of string

(* Type signatures use pointer formatting.  *)
let rec cxx_type_signature_synth_type_to_string typ =
    match typ with
    | Int16 -> "short"
    | Int32 -> "int"
    | Int64 -> "long int"
    | Float16 -> "float16?(unsupported)"
    | Float32 -> "float"
    | Float64 -> "double"
    | Array(stype, dimtype) -> (cxx_type_signature_synth_type_to_string stype) ^ " *"
    | Unit -> "void"
    (* Assume not passed as pointer.   May need to change. *)
    | Struct(sname) -> sname
    | Fun(from, tof) -> raise (CXXGenerationException "Lambdas Unsupported in C++")

let rec cxx_dimtype_to_definition dimtype =
    match dimtype with
            | Dimension(x :: []) -> "[" ^ (name_reference_to_string x) ^ "]"
            | HigherDimention(subdimtype, x :: [])  ->
                    (cxx_dimtype_to_definition subdimtype) ^ "[" ^ (name_reference_to_string x) ^ "]"
            | _ -> raise (CXXGenerationException "Expected individual array types to be selected by the generate pass")


let rec cxx_definition_synth_type_to_string_prefix_postfix typ name =
    match typ with
    | Array(stype, dimtype) ->
            let postfix = cxx_dimtype_to_definition dimtype in
            let prefix, sub_postfix = cxx_definition_synth_type_to_string_prefix_postfix stype name in
            prefix, postfix ^ sub_postfix
    | othertyp ->
            (* If it's another type, then use the simple type generator *)
            (cxx_type_signature_synth_type_to_string othertyp, "")


(* definitions use array formatting so that arrays
   can be allocated on the stack.  *)
let rec cxx_definition_synth_type_to_string typ name =
    let (prefix, postfix) = cxx_definition_synth_type_to_string_prefix_postfix typ name in
    (* Prefix is like the type name, 'name' is the variable name,
       postfix is array markings like [n], and then we need
       to add a semi colon. *)
    prefix ^ " " ^ name ^ postfix ^ ";"

let cxx_names_to_type_definition (typemap: (string, synth_type) Hashtbl.t) names =
    List.map names (fun name -> (cxx_type_signature_synth_type_to_string (Hashtbl.find_exn typemap name)) ^ name)

let rec cxx_generate_from_gir (typemap: (string, synth_type) Hashtbl.t) gir =
    match gir with
    | Definition(nref) ->
            let defntype = (Hashtbl.find_exn typemap (name_reference_to_string nref)) in
            cxx_definition_synth_type_to_string defntype (name_reference_to_string nref)
    | Sequence(girlist) ->
            String.concat ~sep:";\n\t" (List.map girlist (cxx_generate_from_gir typemap))
    | Assignment(fromv, tov) ->
            (cxx_generate_from_lvalue fromv) ^ " = " ^ (cxx_generate_from_rvalue tov)
    | LoopOver(gir, indvariable, loopmax) ->
            let indvar_name = (name_reference_to_string indvariable) in
            let loopmax_name = (name_reference_to_string loopmax) in
            "for (int " ^ indvar_name ^ " = 0; " ^ indvar_name ^ " < " ^ loopmax_name ^ "; " ^ indvar_name ^ "++) {\n\t\t" ^
            (cxx_generate_from_gir typemap gir) ^
            "\n\t}"
    | Expression(expression) ->
            (cxx_generate_from_expression expression)
    | EmptyGIR -> ";"

and cxx_generate_from_lvalue lvalue =
    match lvalue with
    | LVariable(nref) -> (name_reference_to_string nref)
    | LIndex(lval, indexpr) -> (cxx_generate_from_lvalue lval) ^ "["
        ^ (cxx_generate_from_expression indexpr) ^ "]"

and cxx_generate_from_rvalue rvalue =
    match rvalue with
    | Expression(expr) -> (cxx_generate_from_expression expr)
and cxx_generate_from_expression expr =
    match expr with
    | VariableReference(nref) -> (name_reference_to_string nref)
    | ListIndex(nref, expr) -> (cxx_generate_from_expression nref) ^ "[" ^
        (cxx_generate_from_expression expr) ^ "]"
    | FunctionCall(fref, vlist) ->
            (cxx_generate_from_function_ref fref) ^ "(" ^ (cxx_generate_from_vlist vlist) ^ ");"
and cxx_generate_from_function_ref fref =
    match fref with
    | FunctionRef(nref) -> (name_reference_to_string nref)
and cxx_generate_from_vlist vlist =
    match vlist with
    | VariableList(nrefs) ->
        String.concat ~sep:", " (List.map nrefs name_reference_to_string)

let generate_cxx (options: options) (iospec: iospec) program =
    (* C++ only allows for single return values.  *)
    (* This could be ammened to auto-add a struct,
    but can't imagine we'd need that.  *)
    let (function_type, outv) =
        match program.out_variables with
        | [] -> "void", ""
        | x :: [] -> cxx_type_signature_synth_type_to_string (Hashtbl.find_exn program.typemap x), x
        | xs ->
            raise (CXXGenerationException "Can't have multi-output C++ functions")
    in
    (* Generate the function header *)
    let function_header =
        function_type ^ " " ^ iospec.funname ^ "(" ^
        (String.concat ~sep:"," (cxx_names_to_type_definition program.typemap program.in_variables)) ^
        ") {" 
    in
    (* Generate the actual program.  *)
    let program_string = cxx_generate_from_gir program.typemap program.gir in
    (* And generate the return statement *)
    let function_return =
        "return " ^ outv ^ "; }" in
    (* Generate the whole program.  *)
    String.concat ~sep:"\n" [function_header; program_string; function_return]
    (* TODO --- need to include a bunch of unchanging crap, e.g. 
    arg parsing.   I expect that to change /a little/ with
    the argtypes but not much.  *)

let generate_code (options: options) (iospec: iospec) (programs: program list) =
    match options.target with
	| CXX -> List.map programs (generate_cxx options iospec)
