%{
	open Range_definition
%}

%token RANGE
%token COMMA
%token SET
%token LPAREN
%token RPAREN
%token <float> REAL
%token <int> INTEGER
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

item:
	| INTEGER { RangeInteger($1) }
	| REAL { RangeFloat($1) }
