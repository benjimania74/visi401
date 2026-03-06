open Visi
open Lambda
open Lexer
open Parser
open Cam

let repl () =
  let lexbuf = Lexing.from_channel stdin in
  while true do
    Printf.printf "> %!";
    let result = Parser.main Lexer.read lexbuf in
    let value = runCam result in
    Printf.printf "%s\n%!" (show_valeur value)
  done

let () = repl ()
