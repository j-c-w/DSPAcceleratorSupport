%{
	open Spec_definition
%}

%token BOOL
%token INT8
%token INT16
%token INT32
%token INT64
%token UINT8
%token UINT16
%token UINT32
%token UINT64
%token FLOAT16
%token FLOAT32
%token FLOAT64
%token STRING
%token ARRAY
%token POINTER
%token UNIT
%token LPAREN
%token RPAREN
%token ARROW
%token HASH
%token DOT
%token EOF
%token COMMA
%token STAR
%token <string> IDENT
%token <int> INTEGER

%left ARROW

%start  t
%type <Spec_definition.synth_type> t

%%

t:
 | tlist; ARROW; t {Fun($1, $3)}
 | tsub; EOF {$1};

tlist:
 | tsub; { [$1] };
 | tsub; COMMA; tlist { $1 :: $3 };

tident:
 | IDENT { Name($1) }
 | IDENT DOT tident {
     match $3 with
     | StructName(ns) -> StructName(Name($1) :: ns)
     | Name(n) -> StructName([Name($1); Name(n)])
     | AnonymousName -> Name($1)
 }

t_equation_ident:
	 | tident { DimVariable($1, DimEqualityRelation) }
	 | INTEGER { DimConstant($1) }

mul_equation_internal:
	 | t_equation_ident { [$1] }
	 | t_equation_ident STAR mul_equation_internal { $1 :: $3 }

mul_equation:
	 | t_equation_ident STAR mul_equation_internal { MultiDimension($1 :: $3, DimMultiply)  }
	 | t_equation_ident STAR mul_equation_internal { MultiDimension($1 :: $3, DimMultiply) }

t_equation:
	 | mul_equation; { $1 }

tsub:
 | BOOL { Bool }
 | INT8 { Int8 }
 | INT16 { Int16 }
 | INT32 { Int32 }
 | INT64 { Int64 }
 | UINT8 { UInt8}
 | UINT16 { UInt16 }
 | UINT32 { UInt32 }
 | UINT64 { UInt64 }
 | FLOAT16 { Float16 }
 | FLOAT32 { Float32 }
 | FLOAT64 { Float64 }
 | STRING { String }
 | UNIT { Unit }
 | ARRAY; LPAREN; tsub; RPAREN { Array($3, EmptyDimension) };
 | ARRAY; LPAREN; tsub; HASH; INTEGER; RPAREN {  Array($3, SingleDimension(DimConstant($5))) };
 | ARRAY; LPAREN; tsub; HASH; tident; RPAREN { Array($3, SingleDimension(DimVariable($5, DimEqualityRelation))) }
 | ARRAY; LPAREN; tsub; HASH; t_equation; RPAREN { Array($3, $5) };
 | POINTER; LPAREN; tsub; RPAREN { Pointer($3) };
 | IDENT {Struct($1)}
