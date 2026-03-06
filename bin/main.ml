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

(* (λ.(λ.(1 (1 (1 0))))) (λ.(λ.(1 (1 0)))) (λ.0)  *)
(* donne *)
(* 
   λ.1; 1; 0; @; @; ret 
   [ λ.1; 1; 0; @; @; ret 
      [ λ.1; 1; 0; @; @; ret 
         [ λ.0; ret [  ]  ]
      ]  
   ] 
 *)
(* 
   λx. f (f x) [ f ↦ 
                 λx. g (g x) [ g ↦ 
                                 λx. h (h x) [ h ↦ 
                                               λx.x ] ] ]
ie on double trois fois la fonction, c'est ok, mais pas super frappant…
 *)
