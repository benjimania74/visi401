type term = 
  | Ident of int
  | Lam of term | 
  App of term * term
  [@@deriving show]