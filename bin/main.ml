open Visi
open Lambda

let () =
let input = "λ. 0 1 " in
let lexbuf = Lexing.from_string input in
let result = Parser.main Lexer.read lexbuf in
Printf.printf "Res: %s\n" (show_term result)
