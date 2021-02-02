%{
	open Spec_definition
%}

%token INT16
%token INT32
%token INT64
%token FLOAT16
%token FLOAT32
%token FLOAT64
%token ARRAY
%token UNIT
%token LPAREN
%token RPAREN
%token ARROW
%token EOF
%token <string> IDENT

%left ARROW

%start  t
%type <Spec_definition.synth_type> t

%%

t:
 | tsub; ARROW; t {Fun($1, $3)}
 | tsub; EOF {$1};

tsub:
 | INT16 { Int16 }
 | INT32 { Int32 }
 | INT64 { Int64 }
 | FLOAT16 { Float16 }
 | FLOAT32 { Float32 }
 | FLOAT64 { Float64 }
 | UNIT { Unit }
 | ARRAY; LPAREN; tsub; RPAREN { Array($3) };
 | IDENT {Struct($1)}
