open Gir

exception GenerationException of string

let rec fun_generate_c fref =
    match fref with
        | FunctionRef(name) -> name;;

let rec var_generate_c v =
    match v with
        | Variable(name) -> name
        | ListVariable(name, len) -> raise (GenerationException("Can't generate a list variable on its own"));;

let rec vlist_generate_c vlist =
    match vlist with
        | EmptyVariableList -> []
        | VariableList(Variable(name), rest) -> name :: vlist_generate_c rest
        | VariableList(ListVariable(name, lvar), rest) -> (name :: var_generate_c lvar :: vlist_generate_c rest)
;;

let rec gir_generate_c gir_function =
    match gir_function with
        | Sequence(gir1, gir2) ->
                (gir_generate_c gir1) ^ ";\n" ^ (gir_generate_c gir2)
        (* TODO --- Generate the assignment Exprs.  *)
        | Assignment(Variable(name1), Variable(name2), assignment_expr) ->
                name1 ^ " = " ^ name2 ^ ";"
        | Assignment(ListVariable(name1, lenvar1), ListVariable(name2, lenvar2), assignment_expr) ->
                "for (int i = 0; i < " ^ (var_generate_c lenvar1) ^ "; i ++) { " ^
                name1 ^ "[i] = " ^ name2 ^ "[i]; }"
        | Assignment(_, _, _) -> raise (GenerationException("Generation Exception"))
        | Expression(expr) -> raise (GenerationException("Not implemented"))
        | FunctionCall(fref, vars) ->
                (fun_generate_c fref) ^ "(" ^ (String.concat "," (vlist_generate_c vars)) ^ ")"
        | EmptyGIR -> ""
;;
let rec generate_c program =
    match program with
        | Program(input_list, gir, output_live, map) ->
                gir_generate_c gir;;

