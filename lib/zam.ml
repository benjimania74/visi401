open Lambda;;


type instruction_zam = Access of int | MakeClosure of instruction_zam list | TailApply | Apply | Push | PushMark | MakeGrab | Return | BLOCK | PUSH0;;

let ex = App(App(Ident 0, Ident 1), Ident 3)

let rec split (t: term) (args: term list) = match t with
| App(a,b)          ->    split a (b :: args)
| _                 ->    t,args;;



let rec ajout_push_entre (liste: instruction_zam list list): instruction_zam list =
  match liste with
  | []              ->    []
  | x :: xs         ->    x @ (Push :: ajout_push_entre xs);; 

let rec compile_zam (t: term): instruction_zam list = match t with
| Ident n           ->    [Access n]
| Lam l             ->    [ MakeClosure (compileTail l @ [Return] ) ]
| App (a, b) ->  let f, args = split a [b] in 
                          PushMark :: ( ajout_push_entre (List.map compile_zam (List.rev args))  @ (compile_zam f) @ [Apply] ) 
| S                 ->    [BLOCK ; Return]
| Z                 ->    [PUSH0]

 and compileTail (t: term) = match t with
| Ident n           ->    [Access n]
| Lam l             ->    MakeGrab :: (compileTail l )
| App (a, b) ->  let f, args = split a [b] in 
                          ajout_push_entre (List.map compile_zam (List.rev args)) @ (compile_zam f) @ [TailApply]
| S                 ->    [BLOCK ; Return]
| Z                 ->    [PUSH0];;

module Int = struct
  type t = int
  let compare = Stdlib.compare
end;;

type env = valeur list
and
valeur = Code of instruction_zam list | Env of env | Closure of instruction_zam list * env | Value of int | Mark | Grab | MakeVS | VS of valeur | VZ;;

let rec pp_valeur_zam = function 
    | Code instrs -> "⟨" ^ (pp_instrs_zam instrs) ^ "⟩"
    | Env env -> "{" ^ (pp_env_zam env) ^ "}"
    | Closure (instrs,env) -> 
       "λ.( [" ^ (pp_instrs_zam instrs) ^ "], [ " ^ (pp_env_zam env) ^ " ] )"
    | Value n -> string_of_int n
    | MakeVS -> "S"
    | VS v -> "S " ^ (pp_valeur_zam v)
    | VZ -> "Z"
    | Grab -> "G"
    | Mark -> "E"
and pp_instrs_zam instrs = String.concat "; " (List.map pp_instr_zam instrs)
and pp_instr_zam = function 
  | Access n -> string_of_int n
  | MakeClosure instrs -> "Λ.( " ^ (pp_instrs_zam instrs) ^ " )"
  | Apply -> "@"
  | Return -> "ret"
  | TailApply -> "t@"
  | PushMark -> "PE"
  | MakeGrab -> "MG"
  | Push -> "P"
  | BLOCK -> "S"
  | PUSH0 -> "Z"
and pp_env_zam env = String.concat ", " (List.map pp_valeur_zam env)


let pp_option_zam pp_value = function
  | None -> "None"
  | Some v -> "Some (" ^ pp_value v ^ ")"

let pp_valeur_option_zam = pp_option_zam pp_valeur_zam

type pile = valeur Stack.t;;

let rec zam (code: instruction_zam list) (accu: valeur) (env: env) (aStk: pile) (rStk: pile): valeur option =
  let _ = print_string ((pp_instrs_zam code) ^ "    |    " ^ (Stack.fold (fun s v -> s ^ "; " ^ pp_valeur_zam v) "" aStk))
  and _ = print_newline() in
  match code with
    | []                  -> Some( accu )
    | Access n :: c       -> zam c (List.nth env n) env aStk rStk
    | MakeClosure c' :: c -> zam c (Closure(c', env)) env aStk rStk
    | TailApply :: _      ->
      begin match accu with
      | Closure(c', e')   ->
                             zam c' accu ((Stack.pop aStk) :: e') aStk rStk
        | _               -> failwith("TailApply invalide");
      end
    | Apply :: c          -> 
      let v = Stack.pop aStk in
      begin match accu with
        | Closure(c', e') ->
          let _ = Stack.push ( Env(env) ) rStk in
          let _ = Stack.push ( Code(c) ) rStk in
                              zam c' accu (v :: e') aStk rStk
        | MakeVS ->
          let _ = Stack.push (VS v) aStk in
                              zam c accu env aStk rStk 
        | _ -> failwith("Apply invalide");
      end
    | Push :: c -> 
      let _ = Stack.push accu aStk in
                              zam c accu env aStk rStk
    | PushMark :: c ->
      let _ = Stack.push Mark aStk in
                              zam c accu env aStk rStk
    | MakeGrab :: c ->
      let aStkVal = Stack.pop aStk
      in
      begin match aStkVal with
        | Mark -> 
          let c1' = Stack.pop rStk in
          let e1' = Stack.pop rStk in
          begin match (c1', e1') with
            | Code c', Env e' -> 
                              zam c' (Closure(c, env)) e' aStk rStk
            | _            -> failwith("zam : Pas bon MakeGrab 1")
          end
        | _ -> 
          zam c accu (aStkVal :: env) aStk rStk
      end
    | Return :: _ ->
      let v = Stack.pop aStk in
      begin match v with
      | Mark -> 
        let c1' = Stack.pop rStk in
        let e1' = Stack.pop rStk in
        begin match (c1', e1') with
          | Code c', Env e' -> 
            zam c' accu e' aStk rStk
          | _ -> failwith "zam: Pas bon Return 1"
        end
      | _ ->
        begin match accu with
        | Closure(c',e') ->    zam c' accu (v :: e') aStk rStk
        | _              ->    failwith ("zam: Pas bon return 2 " ^ (pp_valeur_zam accu))
        end
      end
    | PUSH0 :: c ->
      zam c VZ env aStk rStk
    | BLOCK :: c ->
      zam c MakeVS env aStk rStk
    
let runZam code = 
  let aS = Stack.create() 
  and rS = Stack.create() in
  zam code (Value 0) [] aS rS;;