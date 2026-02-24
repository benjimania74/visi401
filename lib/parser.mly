%{ 
open Lambda
%}

%token <int> INT
%token LAMBDA "λ"
%token DOT "."
%token LPAREN "("
%token RPAREN ")"
%token EOF


%start <term> main
%%

main:
    expr EOF { $1 }

expr:
    | x = INT; { Ident x }
| expr expr { App($1, $2) }