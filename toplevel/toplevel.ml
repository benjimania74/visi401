open Visi
open Lambda
open Lexer
open Parser
open Cam

let parse_with_error lexbuf =
  try Parser.prog Lexer.token lexbuf with
  | SyntaxError msg ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Syntax error at line %d, column %d: %s\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg;
      exit 1
  | e ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Error at line %d, column %d: %s\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Printexc.to_string e);
      exit 1

let repl () =
  let lexbuf = Lexing.from_channel stdin in
  while true do
    Printf.printf "> %!";
    let result = parse_with_error lexbuf in
    let value = eval result in
    Printf.printf "%s\n%!" (string_of_value value)
  done

let () = repl ()
