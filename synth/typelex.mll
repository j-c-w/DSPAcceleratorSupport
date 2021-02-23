{
	open Typeparse
}

let white = [' ']+
(* Should be fixed to match C++ tokens ideally. *)
(* Well, kind of.  We don't support any 'understanding'
   of templates, so we just treat those as long class
   names.  *)
let ident = ['a'-'z''A'-'Z''_']['<''>''a'-'z''A'-'Z''_''0'-'9']*

rule read =
	parse
	| white {read lexbuf }
	| "int16" {INT16}
	| "int32" {INT32}
	| "int64" {INT64}
	| "float16" {FLOAT16}
	| "float32" {FLOAT32}
	| "float64" {FLOAT64}
	| "array" {ARRAY}
	| "->" {ARROW}
    | ")" { RPAREN }
    | "(" { LPAREN }
	| ident as id { IDENT id }
	| eof {EOF}
