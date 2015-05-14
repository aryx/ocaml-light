%{
open Ast
%}

%token <int> INT
%token PLUS MINUS MULT DIV
%token OPAR CPAR
%token EOF

%start expr
%type <Ast.expr> expr

%%

expr: add_expr EOF  { $1 }
;
add_expr: 
  add_expr PLUS mul_expr  { Plus ($1, $3) }
| add_expr MINUS mul_expr { Minus ($1, $3) }
| mul_expr { $1 }
;
mul_expr:
  mul_expr MULT prim_expr { Mult ($1, $3) }
| mul_expr DIV prim_expr { Div ($1, $3) }
| prim_expr { $1 }
;
prim_expr: 
  INT { Int $1 }
| OPAR add_expr CPAR { $2 }
;
%%
