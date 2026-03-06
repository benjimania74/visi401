open Visi
open Lambda
open Lexer
open Parser

let repl () =
  let lexbuf = Lexing.from_channel stdin in
  while true do
    Printf.printf "> %!";
    let result = Parser.main Lexer.read lexbuf in
    let value = Cam.runCam result in
    Printf.printf "%s\n%!" (show_valeur value)
  done

let () = repl ()

(* open Visi *)
(* open Lambda *)

(* let () = *)
(* let input = "λ. 0 1 " in *)
(* let lexbuf = Lexing.from_string input in *)
(* let result = Parser.main Lexer.read lexbuf in *)
(* Printf.printf "Res: %s\n" (show_term result) *)
