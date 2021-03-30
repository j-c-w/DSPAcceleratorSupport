%{
    open Core_kernel
	open Range_definition
    open Range
%}

%token RANGE
%token COMMA
%token SET
%token LPAREN
%token RPAREN
%token L_SQ_BRACKET
%token R_SQ_BRACKET
%token <float> REAL
%token <int> INTEGER
%token <bool> BOOLEAN
%token EOF

%start t
%type <Range_definition.range_set> t

%%

t:
	| SET LPAREN set_contents RPAREN EOF { RangeSet(Array.of_list $3) }

set_contents:
	| set_item { [$1] }
	| set_item; COMMA; set_contents { $1 :: $3 }

set_item:
	| RANGE LPAREN item COMMA item RPAREN { RangeRange($3, $5) }
	| item { RangeItem($1) }

item_list:
	| item; COMMA; item_list { $1 :: $3 }
	| item { [$1] }

item:
	| INTEGER { RangeInteger($1) }
	| REAL { RangeFloat($1) }
	| BOOLEAN { RangeBool($1) }
	| L_SQ_BRACKET; item_list; R_SQ_BRACKET { RangeArray(range_type_value (List.hd_exn $2), $2) }
