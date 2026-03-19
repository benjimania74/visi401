%{ 
open Lambda
(* open Cam *)
%}

%token <int> INT
%token LAMBDA "λ."
(*
%token DOT "."
*)
%token LPAREN "("
%token RPAREN ")"
%token Z "Z"
%token S "S"
%token EOF

%type <Lambda.term> main 
%start main

%%

main:
    expr EOF { $1 }

element:
    | n=INT; { Ident n }
    | Z ; { Lambda.Z }
    | S ; { Lambda.S }
    | LPAREN; a=expr; RPAREN; { a }

expr:
    | e=element; { e }
    | LAMBDA; e=expr; {Lam e}
    | f=expr; a=element; { App(f,a) }
