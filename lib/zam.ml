open Lambda;;


type instruction_zam = Access of int | MakeClosure of instruction_zam list | TailApply | Apply | PushMark | MakeGrab | Return;;


let rec split (t: term) (args: term list) = match t with
| App(a,b) -> split a (b :: args)
| _ -> t,args;;

let rec compile_zam (t: term) = match t with
| Ident n -> [Access n]
| Lam l -> [ MakeClosure (compileTail (Lam l) )]
| App (a, b) ->  let f, args = split a [b] in PushMark :: ( List.flatten (List.map compile_zam args) @ (compile_zam f) @ [Apply] ) 
| _ -> []
and compileTail (t: term) = match t with
| Ident n -> [Access n ; Return]
| Lam l -> MakeGrab :: (compileTail l )
| App (a, b) ->  let f, args = split a [b] in List.flatten (List.map compile_zam args) @ (compile_zam f) @ [TailApply]
| _ -> [];;

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
       "λ." ^ (pp_instrs_zam instrs) ^ " [ " ^ (pp_env_zam env) ^ " ] "
    | Value n -> string_of_int n
    | MakeVS -> "S"
    | VS v -> "S " ^ (pp_valeur_zam v)
    | VZ -> "Z"
    | Grab -> "G"
    | Mark -> "E"
and pp_instrs_zam instrs = String.concat "; " (List.map pp_instr_zam instrs)
and pp_instr_zam = function 
  | Access n -> string_of_int n
  | MakeClosure instrs -> "Λ." ^ (pp_instrs_zam instrs)
  | Apply -> "@"
  | Return -> "ret"
  | TailApply -> "t@"
  | PushMark -> "PE"
  | MakeGrab -> "MG"
and pp_env_zam env = String.concat ", " (List.map pp_valeur_zam env)


let pp_option_zam pp_value = function
  | None -> "None"
  | Some v -> "Some (" ^ pp_value v ^ ")"

let pp_valeur_option_zam = pp_option_zam pp_valeur_zam

type pile = valeur Stack.t;;

let rec zam (code: instruction_zam list) (accu: valeur) (env: env) (aStk: pile) (rStk: pile): valeur option =
  match code with
    | [] ->
      if Stack.is_empty aStk then
        None
      else Some( Stack.pop aStk )
    | Access n :: c ->
      let accu = List.nth env n in
      zam c accu env aStk rStk
    | MakeClosure c' :: c ->
      let accu = Closure(c', env) in
      zam c accu env aStk rStk
    | TailApply :: _ ->
      begin match accu with
      | Closure(c', e') ->
          let e' = (Stack.pop aStk) :: e' in
          zam c' accu e' aStk rStk
        | _ -> failwith("TailApply invalide");
      end
    | Apply :: c ->
      begin match accu with
        | Closure(c', e') ->
          let _ = Stack.push ( Env(env) ) rStk in
          let _ = Stack.push ( Code(c) ) rStk in
          zam c' e' aStk rStk
        | _ -> failwith("Apply invalide");
      end
    | PushMark :: c ->
      let _ = Stack.push Mark aStk in
      zam c env aStk rStk
    | MakeGrab :: c ->
      let aStkVal = Stack.pop aStk
      in
      begin match aStkVal with
        | Mark -> 
          let c1' = Stack.pop rStk in
          let e1' = Stack.pop rStk in
          begin match (c1', e1') with
            | Code c', Env e' -> 
              let _ = Stack.push ( Closure((MakeGrab :: c), env) ) aStk in
              zam c' e' aStk rStk
            | _ -> failwith("zam : Pas bon MakeGrab 1")
          end
        | _ -> 
          let e = Stack.pop rStk :: env in
          zam code e aStk rStk
      end
    | Return :: _ ->
      let v = Stack.pop aStk in
      begin match v with
      | Closure(c',e') ->
        zam c' e' aStk rStk
      | _ ->
        let v' = Stack.pop aStk in
        begin match v' with
          | Mark -> 
            let c1' = Stack.pop rStk in
            let e1' = Stack.pop rStk in
            begin match (c1', e1') with
              | Code c', Env e' -> 
                let _ = Stack.push v aStk in
                zam c' e' aStk rStk
              | _ -> failwith "zam: Pas bon Return 1"
            end
          | _ -> failwith "zam : Pas bon Return 2"
        end
      end;;
    
let runZam code = 
  let aS = Stack.create() 
  and rS = Stack.create() in
  zam code [] aS rS;;