open Visi
open Cam
open Zam

let repl () =
  let lexbuf = Lexing.from_channel stdin in
  while true do
    Printf.printf "> %!";
    let result = Parser.main Lexer.read lexbuf in
    let value = runCam (compile result) in
    Printf.printf "%s\n%!" (pp_valeur_option value)
  done;;

let replZam () =
    let lexbuf = Lexing.from_channel stdin in
  while true do
    Printf.printf "> %!";
    let result = Parser.main Lexer.read lexbuf in
    let value = runZam (compile_zam result) in
    Printf.printf "%s\n%!" (pp_valeur_option_zam value)
  done;;

if false then
   repl ()
else
   replZam ()

(*
let () = repl ()
*)

(* open Visi *)
(* open Lambda *)

(* let () = *)
(* let input = "λ. 0 1 " in *)
(* let lexbuf = Lexing.from_string input in *)
(* let result = Parser.main Lexer.read lexbuf in *)
(* Printf.printf "Res: %s\n" (show_term result) *)


              (* Exemple d'exponentielle 2**3 *)
(* (λ.(λ.(1 (1 (1 0))))) (λ.(λ.(1 (1 0)))) S Z  *)

              (* Exemple d'exponentielle 3**2 *)
(* (λ.(λ.(1 (1 0)))) (λ.(λ.(1 (1 (1 0))))) S Z *)

              (* Exemple de multiplication 3*2 *)
(* (λ.(λ.(1 (1 0)))) ((λ.(λ.(1 (1 (1 0))))) S) Z *)

              (* Exemple de multiplication 2*3 *)
(* (λ.(λ.(1 (1 (1 0))))) ((λ.(λ.(1 (1 0)))) S) Z *)

              (* Exemple d'addition 2+3 *)
(* ((λ.(λ.(1 (1 (1 0))))) S) (((λ.(λ.(1 (1 0)))) S) Z) *)

              (* Exemple d'addition 3+2 *)
(* ((λ.(λ.(1 (1 0)))) S) (((λ.(λ.(1 (1 (1 0))))) S) Z) *)
