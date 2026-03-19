{
open Parser
}

let digits = ['0'-'9'] (* chaque chiffres *)
let numbers = digits+ (* autant de chiffres qu'on veut à la suite sans espace *)

let spaces = [' ' '\t']+ (* tous les espaces, peu importe s'il y en a un ou deux d'affilés *)



rule read = parse
    | spaces { read lexbuf }
    | numbers { INT (int_of_string (Lexing.lexeme lexbuf)) } (* "Lexing.lexeme lexbuf" extrait la valeur qui a matché avec le regex de numbers*)
    | "λ." { LAMBDA }
    | "λ." { LAMBDA }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | "S" { S }
    | "Z" { Z }
    (* | '.' { DOT } *)
    | eof { EOF }
    | "\n" { EOF }
    | _ { failwith "Caractère inconnu" }
