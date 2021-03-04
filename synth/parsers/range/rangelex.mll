{
    open Rangeparse
}

let white = [' ']+
(* TODO --- would like to support chars etc. also. *)
let real = '-'?['0'-'9']+'.'['0'-'9']*
let integer = '-'?['0'-'9']+

rule read =
    parse
        | white {read lexbuf}
        | "range" {RANGE}
        | "set" {SET}
        | "(" {LPAREN}
        | ")" {RPAREN}
        | "," {COMMA}
        | real as r { REAL (float_of_string r) }
        | integer as i { INTEGER (int_of_string i) }
        | eof { EOF }