open Visi
open Cam

let repl () =
  let lexbuf = Lexing.from_channel stdin in
  while true do
    Printf.printf "> %!";
    let result = Parser.main Lexer.read lexbuf in
    let value = runCam (compile result) in
    Printf.printf "%s\n%!" (pp_valeur_option value)
  done

let () = repl ()

(* open Visi *)
(* open Lambda *)

(* let () = *)
(* let input = "λ. 0 1 " in *)
(* let lexbuf = Lexing.from_string input in *)
(* let result = Parser.main Lexer.read lexbuf in *)
(* Printf.printf "Res: %s\n" (show_term result) *)
