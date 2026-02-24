(*%{ 
open Lambda
%}*)

%token <int> INT
%token PLUS
%token EOF

%start <int> main
%%

main:
    expr EOF { $1 }

expr:
    INT { $1 }
| expr PLUS expr { $1 + $3 }