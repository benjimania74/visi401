open Lambda;;

type instruction = Access of int | MakeClosure of instruction list | Apply | Return | BLOCK | PUSH0
[@@deriving show];;

let rec compile (t: term) = match t with
  | Ident n -> [Access n]
  | Lam l -> [MakeClosure (List.append (compile l) [Return]) ]
  | App (a, b) -> List.flatten [ compile a ; compile b ; [Apply] ]
  | S -> [BLOCK]
  | Z -> [PUSH0];; 

module Int = struct
  type t = int
  let compare = Stdlib.compare
end;;

type env = valeur list
and
valeur = Code of instruction list | Env of env | Closure of instruction list * env | Value of int | MakeVS | VS of valeur | VZ
[@@deriving show]
;;

(* let%show : valeur option -> string = function *)
(*   | None -> "None" *)
(*   | Some v -> "Some (" ^ show_valeur v ^ ")" *)


let rec pp_valeur = function 
    | Code instrs -> "⟨" ^ (pp_instrs instrs) ^ "⟩"
    | Env env -> "{" ^ (pp_env env) ^ "}"
    | Closure (instrs,env) -> 
       "λ." ^ (pp_instrs instrs) ^ " [ " ^ (pp_env env) ^ " ] "
    | Value n -> string_of_int n
    | MakeVS -> "S"
    | VS v -> "S " ^ (pp_valeur v)
    | VZ -> "Z"
and pp_instrs instrs = String.concat "; " (List.map pp_instr instrs)
and pp_instr = function 
  | Access n -> string_of_int n
  | MakeClosure instrs -> "Λ." ^ (pp_instrs instrs)
  | Apply -> "@"
  | Return -> "ret"
  | BLOCK -> "S"
  | PUSH0 -> "Z"
and pp_env env = String.concat ", " (List.map pp_valeur env)


let pp_option pp_value = function
  | None -> "None"
  | Some v -> "Some (" ^ pp_value v ^ ")"

let pp_valeur_option = pp_option pp_valeur


type pile = valeur Stack.t;;

let rec cam (code: instruction list) (env: env) (pile: pile): valeur option =
    match code with
      | [] -> 
        if Stack.is_empty pile then
            None 
        else Some( Stack.pop pile )
      | Access n :: c -> 
        let _ = Stack.push ( List.nth env n ) pile in
        cam c env pile
      | MakeClosure c' :: c ->
        let _ = Stack.push ( Closure(c', env) ) pile in
        cam c env pile 
      | Apply :: c ->
        let value = Stack.pop pile in
        let value' = Stack.pop pile in
        
        begin match value' with
            | Closure (closureCode, closureEnv) ->
                let _ = Stack.push ( Env(env) ) pile
                in
                let _ = Stack.push ( Code(c) ) pile
                in
                let closureEnv = value :: closureEnv
                in
                cam closureCode closureEnv pile
            | MakeVS ->
              let _ = Stack.push (VS value) pile
              in
              cam c env pile
            | _ -> failwith("Apply invalide")
        end
      | Return :: _ ->
        let ret = Stack.pop pile in 
        let resumeCode = Stack.pop pile in 
        let pastEnv = Stack.pop pile in
        begin match resumeCode, pastEnv with
            | Code code, Env env ->
              let _ = Stack.push ret pile
              in
              cam code env pile
            | _ ->
              failwith("Return invalide")
        end
      | PUSH0 :: c ->
        let _ = Stack.push VZ pile
        in
        cam c env pile
      | BLOCK :: c ->
        let _ = Stack.push MakeVS pile
        in
        cam c env pile;;
        
    
let runCam code = 
  let s = Stack.create() in
  cam code [] s;;

let rec trans_n (n : int) = 
  if n < 0 then failwith("Pas bon")
  else
    if n = 0 then Ident 0
    else App( Ident 1, trans_n (n-1) );;
  
let church (n: int) = Lam( Lam (trans_n n) );;

let zero = church 0;;
let trois = church 3;;
let deux = church 2;;
(*λnmfx.nf(mfx)*)

let add_church = Lam(
                  Lam(
                    Lam(
                      Lam(
                        App(
                          App(
                            Ident 3,
                            Ident 1
                          ),
                          App(
                            App(
                              Ident 2, 
                              Ident 1
                            ), 
                            Ident 0
                          )
                        )
                      ) 
                    )
                  )
                );;

let ex = App( App(add_church, trois), deux);;
