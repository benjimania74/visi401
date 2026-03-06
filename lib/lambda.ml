type term = 
  | Ident of int
  | Lam of term 
  | App of term * term
  [@@deriving show]

type program = Program of term list

let show_program (program: program): string =
  match program with
    | Program p ->
      let instruction = List.nth p 0 in
      show_term instruction

(*
type token = 
  | Function
  | Number of int
  | LParen
  | RParen
  | ApplyFunc

type token_program = TProgram of token list
*)
(* λ 0 . 2 . λ 0 1*)
(* λ 0 1 *)
(* λ λ λ λ(3.1)( (2.1).0 ) *)

(*
(Lambda.Lam (
  Lambda.App (
    (Lambda.Ident 0),
    (Lambda.Ident 1)
  )
))

(
Lambda.Lam(
  Lambda.App(
    (
      Lambda.App(
        (
          Lambda.Ident 0
        ),
        (
          Lambda.Ident 1
        )
      )
    ),
    (
      Lambda.Ident 2
    )
  )
)
)
*)