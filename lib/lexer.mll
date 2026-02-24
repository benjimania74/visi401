{
open Parser
}

rule read = parse
    | [' ' '\t' '\n'] { read lexbuf }
    | "λ" { LAMBDA }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | eof { EOF }
    | _ { failwith "Caractère inconnu" }