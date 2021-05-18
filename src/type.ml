open Syntax
open ContractId


let id_lookup_ty  
      (contract_interfaces : Contract.contract_interface with_cid)
      (tenv : TypeEnv.type_env) id : (ty * SideEffect.t list) expr =
  match TypeEnv.lookup tenv id with
  | Some (typ, Some loc) -> (IdentifierExpr id, (typ, [loc, SideEffect.Read]))
  | Some (typ, None) -> (IdentifierExpr id, (typ, []))
  | None -> failwith ("unknown identifier "^id)
  (* what should it return when it is a method name? *)

let is_known_contract contract_interfaces name =
  BatList.exists (fun (_, i) -> i.Contract.contract_intf_name = name) contract_interfaces

let rec is_known_ty ( contract_interfaces : Contract.contract_interface with_cid) = Syntax.(function 
    | TyRef l                       ->  BatList.for_all (is_known_ty contract_interfaces) l
    | TyTuple l                     ->  BatList.for_all (is_known_ty contract_interfaces) l
    | TyMap(a, b)                   ->  is_known_ty contract_interfaces a && is_known_ty contract_interfaces b
    | TyContractArch contract       ->  is_known_contract contract_interfaces contract
    | TyContractInstance contract   ->  is_known_contract contract_interfaces contract
    | TyUint256                     ->  true
    | TyUint8                       ->  true
    | TyBytes32                     ->  true
    | TyAddr                        ->  true
    | TyBool                        ->  true
    | TyVoid                        ->  true  )

let arg_has_known_ty contract_interfaces arg =
  let ret = is_known_ty contract_interfaces arg.ty in
  if not ret then Printf.eprintf "arg has an unknown type %s\n" (Syntax.string_of_ty arg.ty);
  ret

let ret_type_is_known contract_interfaces header =
  BatList.for_all (is_known_ty contract_interfaces) header.mthd_ret_ty

let assign_type_mthd_head contract_interfaces header =
  match header with
  | Method header ->
     let () = assert (BatList.for_all (arg_has_known_ty contract_interfaces) header.mthd_args) in
     let () = assert (ret_type_is_known contract_interfaces header) in
     Method header
  | Default ->
     Default

let call_arg_expectations (contract_interfaces : Contract.contract_interface with_cid) mtd : ty list -> bool =
  match mtd with
  | "pre_ecdsarecover"  ->  (fun x -> x = [TyBytes32; TyUint8; TyBytes32; TyBytes32])
  | "keccak256"         ->  (fun _ -> true)
  | "iszero"            ->  (fun x -> x = [TyBytes32] || x = [TyUint8] || x = [TyUint256] || x = [TyBool] || x = [TyAddr] )
  | name                ->
        let cid = lookup_id (fun c -> c.Contract.contract_intf_name = name) contract_interfaces in
        let interface : Contract.contract_interface = choose_contract cid contract_interfaces in
        (fun x -> x = interface.Contract.contract_intf_args)

let type_check ((expr : ty), ((_,(t, _)) : (ty * 'a) expr)) =
  assert (expr = t)

let check_args_match (contract_interfaces : Contract.contract_interface with_cid) (args : (ty * 'x) expr list) (call_head : string option) =
    let expectations : ty list -> bool =   match call_head with
        | Some mtd      ->   call_arg_expectations contract_interfaces mtd
        | None          ->  (fun x -> x = [])
    in
    assert (expectations (List.map (fun x -> (fst (snd x))) args))

let typecheck_multiple (exprs : ty list) (actual : (ty * 'a) expr list) =
    List.for_all2 (fun e (_, (a, _)) -> e = a) exprs actual

let check_only_one_side_effect (llst : SideEffect.t list list)  =
    (* write-write *)
    let ll = List.filter (fun x-> BatList.exists (fun s-> snd s = SideEffect.Write)x) llst in 
    if List.length ll > 1 then failwith "more than one sub-expressions have side-effects";
    (* read-write *)
    if List.length ll = 0 then () else if List.length (List.filter (fun x ->
                      BatList.exists (fun s -> snd s = SideEffect.Read) x
                    ) llst) > 0 then
        failwith "some sub-expressions have write effects and some have read effects"

let has_no_side_effects (e : (ty * SideEffect.t list) expr) =
  snd (snd e) = []

let rec assign_type_call
      contract_interfaces
      cname
      venv (src : unit function_call) : ((ty * SideEffect.t list) function_call * (ty * SideEffect.t list)) =
  let args' = List.map (assign_type_expr contract_interfaces cname venv)
                       src.call_args in
  let () = check_args_match contract_interfaces args' (Some src.call_head) in
  let args_side_effects : SideEffect.t list list = List.map (fun (_, (_, s)) -> s) args' in
  let () = check_only_one_side_effect args_side_effects in
  let side_effects = (SideEffect.External, SideEffect.Write) :: List.concat args_side_effects in
  let ret_typ =
    match src.call_head with
    | "value" when true (* check the arg is 'msg' *) 
                                -> TyUint256
    | "pre_ecdsarecover"        -> TyAddr
    | "keccak256"               -> TyBytes32
    | "iszero"                  -> (match args' with
                   | [arg]          -> TyBool
                   | _              -> failwith "should not happen")
    | contract_name when true (* check the contract exists*) 
                                -> TyContractArch contract_name
    | _                         -> failwith "assign_type_call: should not happen" in
  ({    call_head       = src.call_head; 
        call_args       = args' },
   (ret_typ, side_effects))


and assign_type_msg_info contract_interfaces cname tenv
                             (orig : unit msg_info) : (ty * SideEffect.t list) msg_info =
  let v' = BatOption.map (assign_type_expr contract_interfaces cname tenv)
                            orig.msg_value_info in
  let block' = assign_type_stmts contract_interfaces cname tenv orig.msg_reentrance_info in
  { msg_value_info      = v'
  ; msg_reentrance_info = block' }


and assign_type_expr
      (contract_interfaces : Contract.contract_interface with_cid)
      (cname : string)
      (venv : TypeEnv.type_env) ((expr_inner, ()) : unit expr) : (ty * SideEffect.t list) expr =
  match expr_inner with
  | ThisExpr             -> (ThisExpr,    (TyContractInstance cname, []))
  | TrueExpr             -> (TrueExpr,    (TyBool, []))
  | FalseExpr            -> (FalseExpr,   (TyBool, []))
  | SenderExpr           -> (SenderExpr,  (TyAddr, []))
  | NowExpr              -> (NowExpr,     (TyUint256, []))
  | FunctionCallExpr c   ->
     let (c', typ) = assign_type_call contract_interfaces cname venv c in
     (FunctionCallExpr c', typ)
  | DecLit256Expr d      -> (DecLit256Expr d, (TyUint256, []))
  | DecLit8Expr d        -> (DecLit8Expr d,   (TyUint8, []))
  | IdentifierExpr s     ->
     (* Now something is strange. This might not need a type anyway. *)
     (* Maybe introduce a type called CallableType *)
     let () =
       if BatString.starts_with s "pre_" then
         failwith "names that start with pre_ are reserved" in
     id_lookup_ty contract_interfaces venv s
  | ParenthExpr e        ->
     (* omit the parenthesis at this place, the tree already contains the structure *)
     assign_type_expr contract_interfaces cname venv e
  | NewExpr n            ->
     let (n', contract_name) = assign_type_new_expr contract_interfaces cname venv n in
     let () =
       if BatString.starts_with contract_name "pre_" then
         failwith "names that start with pre_ are reserved" in
     (NewExpr n', (TyContractInstance contract_name, [SideEffect.External, SideEffect.Write]))
  | LandExpr (l, r)      ->
     let l  = assign_type_expr contract_interfaces cname venv l in
     let () = type_check (TyBool, l) in
     let r  = assign_type_expr contract_interfaces cname venv r in
     let () = type_check (TyBool, r) in
     let sides = (List.map (fun (_, (_, x)) -> x) [l; r]) in
     let () = check_only_one_side_effect sides in
     (LandExpr (l, r), (TyBool, List.concat sides))
  | LtExpr (l, r)        ->
     let l = assign_type_expr contract_interfaces cname venv l in
     let r = assign_type_expr contract_interfaces cname venv r in
     let sides = (List.map (fun (_, (_, x)) -> x) [l; r]) in
     let () = check_only_one_side_effect sides in
     let () = assert (fst (snd l) = fst (snd r)) in
     (LtExpr (l, r), (TyBool, List.concat sides))
  | GtExpr (l, r)        ->
     let l' = assign_type_expr contract_interfaces cname venv l in
     let r' = assign_type_expr contract_interfaces cname venv r in
     let () = assert (fst (snd l') = fst (snd r')) in
     let sides = (List.map (fun (_, (_, x)) -> x) [l'; r']) in
     let () = check_only_one_side_effect sides in
     (GtExpr (l', r'), (TyBool, List.concat sides))
  | NeqExpr (l, r)       ->
     let l = assign_type_expr contract_interfaces cname venv l in
     let r = assign_type_expr contract_interfaces cname venv r in
     let () = assert (fst (snd l) = fst (snd r)) in
     let sides = (List.map (fun (_, (_, x)) -> x) [l; r]) in
     let () = check_only_one_side_effect sides in
     (NeqExpr (l, r), (TyBool, List.concat sides))
  | EqualityExpr (l, r)  ->
     let l = assign_type_expr contract_interfaces cname venv l in
     let r = assign_type_expr contract_interfaces cname venv r in
     let () = assert (fst (snd l) = fst (snd r)) in
     let sides = (List.map (fun (_, (_, x)) -> x) [l; r]) in
     let () = check_only_one_side_effect sides in
     (EqualityExpr (l, r), (TyBool, List.concat sides))
  | PlusExpr (l, r)      ->
     let l = assign_type_expr contract_interfaces cname venv l in
     let r = assign_type_expr contract_interfaces cname venv r in
     let () = if (snd l <> snd r) then
                (Printf.printf "%s %s\n%!" (string_of_ty (fst (snd l)))
                               (string_of_ty (fst (snd r))))
     in
     let () = assert (fst (snd l) = fst (snd r)) in
     let sides = (List.map (fun (_, (_, x)) -> x) [l; r]) in
     let () = check_only_one_side_effect sides in
     (PlusExpr (l, r), (fst (snd l), List.concat sides))
  | MinusExpr (l, r)     ->
     let l = assign_type_expr contract_interfaces cname venv l in
     let r = assign_type_expr contract_interfaces cname venv r in
     let () = assert (fst (snd l) = fst (snd r)) in
     let sides = (List.map (fun (_, (_, x)) -> x) [l; r]) in
     let () = check_only_one_side_effect sides in
     (MinusExpr (l, r), (fst (snd l), List.concat sides))
  | MultExpr (l, r)      ->
     let l = assign_type_expr contract_interfaces cname venv l in
     let r = assign_type_expr contract_interfaces cname venv r in
     let () = assert (fst (snd l) = fst (snd r)) in
     (MultExpr (l, r), snd l)
  | NotExpr negated      ->
     let negated = assign_type_expr contract_interfaces cname venv negated in
     let () = assert (fst (snd negated) = TyBool) in
     (NotExpr negated, (TyBool, snd (snd negated)))
  | AddressExpr inner    ->
     let inner' = assign_type_expr contract_interfaces cname venv inner in
     (AddressExpr inner', (TyAddr, snd (snd inner')))
  | BalanceExpr inner    ->
     let inner = assign_type_expr contract_interfaces cname venv inner in
     let () = assert (acceptable_as TyAddr (fst (snd inner))) in
     let () = assert (snd (snd inner) = []) in
     (BalanceExpr inner, (TyUint256, [SideEffect.External, SideEffect.Read]))
  | ArrayAccessExpr aa   ->
     let atyped = assign_type_expr contract_interfaces cname venv (read_array_access aa).array_access_array in
     begin match fst (snd atyped) with
     | TyMap(kT, vT) ->
        let (idx', (idx_typ', idx_side')) = assign_type_expr contract_interfaces cname venv (read_array_access aa).array_access_index in
        let () = assert (acceptable_as kT idx_typ') in
        let () = assert (BatList.for_all (fun x -> x = (SideEffect.Storage, SideEffect.Read)) idx_side') in
        (* TODO Check idx_typ' and key_type are somehow compatible *)
        (ArrayAccessExpr (ArrayAccessLExpr
           { array_access_array = atyped
           ; array_access_index = (idx', (idx_typ', idx_side'))
           }), (vT, [SideEffect.Storage, SideEffect.Read]))
     | _                    -> failwith "index access has to be on mappings"
     end
  | SendExpr send        ->
     let msg_info' = assign_type_msg_info contract_interfaces cname venv send.send_msg_info in
     let contract' = assign_type_expr contract_interfaces cname venv send.send_head_contract in
     begin match send.send_head_method with
     | Some mtd ->
        let contract_name = Syntax.contract_name_of_instance contract' in
        let method_sig : Ethereum.function_signature = begin
            match Contract.find_method_signature
                    contract_interfaces contract_name mtd with
            | Some x -> x
            | None -> failwith ("method "^mtd^" not found")
          end
        in
        let types = Ethereum.(List.map to_ty (method_sig.sig_return)) in
        let args = List.map (assign_type_expr contract_interfaces cname venv)
                                     send.send_args in
        let () = assert (BatList.for_all has_no_side_effects args) in
        let reference : (ty * SideEffect.t list) expr =
          ( SendExpr
              { send_head_contract = contract'
              ; send_head_method = send.send_head_method
              ; send_args = args
              ; send_msg_info = msg_info'
              },
            (TyRef types, [SideEffect.External, SideEffect.Write])
          ) in
        (match types with
         | [single] -> (SingleDerefExpr reference, (single, [SideEffect.External, SideEffect.Write]))
         | _ -> reference)
     | None ->
        let () = assert (send.send_args = []) in
        ( SendExpr
            { send_head_contract = contract'
            ; send_head_method = None
            ; send_args = []
            ; send_msg_info = msg_info'
            }, (TyVoid, [SideEffect.External, SideEffect.Write]) )
     end
  | ValueExpr                -> (ValueExpr, (TyUint256, []))
  | SingleDerefExpr _
  | TupleDerefExpr _   ->  failwith "DerefExpr not supposed to appear in the raw tree for now"


and assign_type_new_expr
      contract_interfaces
      (cname : string)
      (tenv : TypeEnv.type_env)
      (e : unit new_expr) : ((ty * SideEffect.t list) new_expr * string (* name of the contract just created *)) =
  let msg_info' = assign_type_msg_info contract_interfaces cname tenv e.new_msg_info in
  let args' = List.map (assign_type_expr contract_interfaces cname tenv) e.new_args in
  let e' =
    { new_head = e.new_head
    ; new_args = args'
    ; new_msg_info = msg_info'
    } in
  (e', e.new_head )

  
and assign_type_lexpr
      contract_interfaces
      (cname : string)
      venv (src : unit lexpr) : (ty * SideEffect.t list) lexpr =
  (* no need to type the left hand side? *)
  match src with
  | ArrayAccessLExpr aa ->
     let atyped = assign_type_expr contract_interfaces cname venv aa.array_access_array in
     begin match fst (snd atyped) with
     | TyMap (kT, vT) ->
        let (idx', idx_typ') = assign_type_expr contract_interfaces
                                               cname venv
                                               aa.array_access_index in
        (* TODO Check idx_typ' and key_type are somehow compatible *)
        (ArrayAccessLExpr
           { array_access_array = atyped
           ; array_access_index = (idx', idx_typ')})
     | _ -> failwith ("unknown array")
     end


and assign_type_return
      (contract_interfaces : Contract.contract_interface with_cid)
      (cname : string)
      (tenv : TypeEnv.type_env)
      (src : unit return) : (ty * SideEffect.t list) return =
  let exprs = BatOption.map (assign_type_expr contract_interfaces
                                   cname tenv) src.ret_expr in
  let f = TypeEnv.lookup_expected_returns tenv in
  let () = assert (f (BatOption.map (fun x -> (fst (snd x))) exprs)) in
  { ret_expr = exprs
  ; ret_cont =  assign_type_expr contract_interfaces
                                   cname tenv src.ret_cont
  }


and type_var_declare
      contract_interfaces cname tenv (vi : unit var_declare) :
      ((ty * SideEffect.t list) var_declare * TypeEnv.type_env) =
  (* This function has to enlarge the type environment *)
  let value' = assign_type_expr contract_interfaces
                               cname tenv vi.var_declare_value in
  let added_name = vi.var_declare_name in
  let () =
    if BatString.starts_with added_name "pre_" then
      failwith "names that start with pre_ are reserved" in
  let added_typ = vi.var_declare_type in
  let () = assert (is_known_ty contract_interfaces added_typ) in
  let new_env = TypeEnv.add_pair tenv added_name added_typ None in
  let new_init =
    { var_declare_type = added_typ
    ; var_declare_name = added_name
    ; var_declare_value = value'
    } in
  (new_init, new_env)


and assign_type_stmt
      (contract_interfaces : Contract.contract_interface with_cid)
      (cname : string)
      (venv : TypeEnv.type_env)
      (src : unit stmt) :
      ((ty * SideEffect.t list) stmt * TypeEnv.type_env (* updated environment *)) =
  match src with
  | AbortStmt -> (AbortStmt, venv)
  | ReturnStmt r ->
     let r' =
       assign_type_return contract_interfaces cname venv r in
     (ReturnStmt r', venv)
  | AssignStmt (l, r) ->
     let l' = assign_type_lexpr contract_interfaces cname venv l in
     let r' = assign_type_expr contract_interfaces cname venv r in
     (AssignStmt (l', r'), venv)
  | IfThenOnly (cond, ss) ->
     let cond' = assign_type_expr contract_interfaces cname venv cond in
     let ss'
       = assign_type_stmts
           contract_interfaces cname venv ss in
     (IfThenOnly (cond', ss'), venv)
  | IfThenElse (cond, sst, ssf) ->
     let cond' = assign_type_expr contract_interfaces cname venv cond in
     let sst' = assign_type_stmts contract_interfaces cname venv sst in
     let ssf' = assign_type_stmts contract_interfaces cname venv ssf in
     (IfThenElse (cond', sst', ssf'), venv)
  | SelfDestructStmt e ->
     let e' = assign_type_expr contract_interfaces cname venv e in
     (SelfDestructStmt e', venv)
  | VarDeclStmt vi ->
     let (vi', venv') =  type_var_declare contract_interfaces cname venv vi in
     (VarDeclStmt vi', venv')
  | ExprStmt expr ->
     let expr = assign_type_expr contract_interfaces cname venv expr in
     let () = assert (fst (snd expr) = TyVoid) in
     let () = assert (BatList.exists (fun (_, x) -> x = SideEffect.Write) (snd (snd expr))) in
     (ExprStmt expr, venv)
  | LogStmt (name, args, _) ->
     let args = List.map (assign_type_expr contract_interfaces cname venv) args in
     let event = TypeEnv.lookup_event venv name in
     let type_expectations =
       List.map (fun ea -> Syntax.(ea.event_arg_body.ty)) event.Syntax.event_args in
     let () = assert (typecheck_multiple type_expectations args) in
     let side_effects = List.map (fun (_, (_, a)) -> a) args in
     let () = check_only_one_side_effect side_effects in
     (LogStmt (name, args, Some event), venv)


and assign_type_stmts
          (contract_interfaces : Contract.contract_interface with_cid)
          (cname : string)
          (type_environment : TypeEnv.type_env)
          (ss : unit stmt list) : (ty * SideEffect.t list) stmt list =
  match ss with
  | [] -> []
  | first_s :: rest_ss ->
     let (first_s', (updated_environment : TypeEnv.type_env)) =
       assign_type_stmt
         contract_interfaces cname type_environment first_s in
     first_s' :: assign_type_stmts contract_interfaces
                                       cname
                                       updated_environment
                                       rest_ss



type termination =
    | RunAway 
    | ReturnValues of int 
    | JustStop

let rec is_terminating_stmt (s : unit stmt) : termination list = match s with
    | AbortStmt         ->  [JustStop]
    | ReturnStmt ret    ->  begin match ret.ret_expr with
        | Some _    -> [ReturnValues 1]
        | None      -> [ReturnValues 0] end
  | AssignStmt _    ->  [RunAway]
  | VarDeclStmt _  ->  [RunAway]
  | IfThenOnly (_, b)       ->  (are_terminating b) @ [RunAway] (* there is a continuation if the condition does not hold. *)
  | IfThenElse (_, bT, bF)  ->  are_terminating bT @ (are_terminating bF)
  | SelfDestructStmt _  ->  [JustStop]
  | ExprStmt _           ->  [RunAway]
  | LogStmt _           ->  [RunAway]

(** [check_termination stmts] make sure that the last stmt in [stmts]
 *  cuts the continuation. *)
and are_terminating stmts =
  let last_stmt = BatList.last stmts in
  is_terminating_stmt last_stmt

let mthd_is_returning_void (mthd : unit mthd) : bool = 
    match mthd.mthd_head with
    | Default       ->  true
    | Method m      ->  m.mthd_ret_ty = []

let return_expectation_of_mthd (h : Syntax.mthd_head) (actual : ty option) : bool =
    match h, actual with
    | Default,Some _  ->  false
    | Default,None    ->  true
    | Method u, _      ->  begin match u.mthd_ret_ty, actual with
        | _ :: _ :: _, _    -> false
        | [x], Some y       -> Syntax.acceptable_as x y
        | [], None          -> true
        | _, _              ->false  end


let assign_type_mthd (contract_interfaces : Contract.contract_interface with_cid)
                     (contract_name : string)
                     (venv : TypeEnv.type_env)
                     (mthd : unit mthd) =
  let () = assert (List.for_all (fun t -> match t with
                       | RunAway -> false
                       | ReturnValues 0 -> mthd_is_returning_void mthd
                       | ReturnValues 1 -> not (mthd_is_returning_void mthd)
                       | ReturnValues _ -> failwith "returning multiple values not supported yet"
                       | JustStop       -> true )  (are_terminating mthd.mthd_body)) in
  let mthd_args = mthd_head_arg_list mthd.mthd_head in
  let () =
    if BatList.exists (fun arg -> BatString.starts_with arg.id "pre_") mthd_args then
      failwith "names that start with pre_ are reserved" in
  let returns : ty option -> bool = return_expectation_of_mthd mthd.mthd_head in
  { mthd_head = assign_type_mthd_head contract_interfaces mthd.mthd_head
  ; mthd_body   = assign_type_stmts contract_interfaces contract_name
            (TypeEnv.remember_expected_returns (TypeEnv.add_block mthd_args venv) returns)
            mthd.mthd_body }

let has_distinct_signatures (c : unit Syntax.contract) : bool =
  let mthds         =   c.mthds in
  let signatures    =   List.map (fun c -> match c.mthd_head with
                            | Method m -> Some (Ethereum.string_of_mthd_info_ty m)
                            | Default  -> None) mthds in
  let unique_sig    =   BatList.unique signatures in
  List.length signatures = List.length unique_sig


let assign_type_contract (env : Contract.contract_interface with_cid)
                         (events: event with_cid)
      (raw : unit Syntax.contract) :
      (ty * SideEffect.t list) Syntax.contract =
  let ()    = assert (BatList.for_all (arg_has_known_ty env) raw.contract_args) in
  let ()    = assert (has_distinct_signatures raw) in
  let tenv  = TypeEnv.(add_block raw.contract_args (add_events events empty_type_env)) in
  let ()    = if BatString.starts_with raw.contract_name "pre_" then
                failwith "names that start with pre_ are reserved" in
  let ()    = if BatList.exists (fun arg -> BatString.starts_with arg.id "pre_") raw.contract_args then
                failwith "names that start with pre_ are reserved" in
  { contract_name       = raw.contract_name
  ; contract_args  = raw.contract_args
  ; mthds      =
      List.map (assign_type_mthd env raw.contract_name tenv) raw.mthds }


let assign_type_toplevel (interfaces : Contract.contract_interface with_cid)
                         (events : event with_cid)
                         (raw : unit Syntax.toplevel) :
      (ty * SideEffect.t list) Syntax.toplevel = match raw with
  | Contract c  -> Contract (assign_type_contract interfaces events c)
  | Event e     -> Event e



(* XXX: these [strip_side_effects_X] should be generalized over any f : 'a -> 'b *)

let rec strip_side_effects_stmt (raw : (ty * 'a) stmt) : ty stmt =
  match raw with
  | AbortStmt               -> AbortStmt
  | ReturnStmt ret          -> ReturnStmt (strip_side_effects_return ret)
  | AssignStmt (l, r)   ->
     AssignStmt (strip_side_effects_lexpr l, strip_side_effects_expr r)
  | VarDeclStmt v      -> VarDeclStmt (strip_side_effects_var_declare v)
  | IfThenOnly (e, block)       -> IfThenOnly (strip_side_effects_expr e, strip_side_effects_mthd_body block)
  | IfThenElse (e, b0, b1)      -> IfThenElse ((strip_side_effects_expr e), (strip_side_effects_mthd_body b0),
                                    (strip_side_effects_mthd_body b1))
  | SelfDestructStmt e      -> SelfDestructStmt (strip_side_effects_expr e)
  | ExprStmt e               -> ExprStmt (strip_side_effects_expr e)
  | LogStmt(str, args, eopt)-> LogStmt (str, List.map strip_side_effects_expr args, eopt)


and strip_side_effects_var_declare v =
  { var_declare_type = v.var_declare_type
  ; var_declare_name = v.var_declare_name
  ; var_declare_value = strip_side_effects_expr v.var_declare_value
  }


and strip_side_effects_aa aa =
  { array_access_array = strip_side_effects_expr aa.array_access_array
  ; array_access_index = strip_side_effects_expr aa.array_access_index
  }


and strip_side_effects_lexpr lexpr =
  match lexpr with
  | ArrayAccessLExpr aa          -> ArrayAccessLExpr (strip_side_effects_aa aa)


and strip_side_effects_expr (i, (t, _)) =
  (strip_side_effects_expr_inner i, t)


and strip_side_effects_function_call fc =
  { call_head = fc.call_head
  ; call_args = List.map strip_side_effects_expr fc.call_args
  }


and strip_side_effects_msg_info m =
  { msg_value_info = BatOption.map strip_side_effects_expr m.msg_value_info
  ; msg_reentrance_info =
      List.map strip_side_effects_stmt m.msg_reentrance_info
  }


and strip_side_effects_send s =
  { send_head_contract = strip_side_effects_expr s.send_head_contract
  ; send_head_method = s.send_head_method
  ; send_args = List.map strip_side_effects_expr s.send_args
  ; send_msg_info = strip_side_effects_msg_info s.send_msg_info
  }


and strip_side_effects_new_expr n =
  { new_head = n.new_head
  ; new_args = List.map strip_side_effects_expr n.new_args
  ; new_msg_info = strip_side_effects_msg_info n.new_msg_info
  }


and strip_side_effects_expr_inner i =
  match i with
  | TrueExpr                 -> TrueExpr
  | FalseExpr                -> FalseExpr
  | DecLit256Expr d          -> DecLit256Expr d
  | DecLit8Expr d            -> DecLit8Expr d
  | NowExpr                  -> NowExpr
  | FunctionCallExpr fc      -> FunctionCallExpr (strip_side_effects_function_call fc)
  | IdentifierExpr str       -> IdentifierExpr str
  | ParenthExpr e            -> ParenthExpr (strip_side_effects_expr e)
  | NewExpr e                -> NewExpr (strip_side_effects_new_expr e)
  | SendExpr send            -> SendExpr (strip_side_effects_send send)
  | LandExpr (a, b)          -> LandExpr (strip_side_effects_expr a, strip_side_effects_expr b)
  | LtExpr (a, b)            -> LtExpr (strip_side_effects_expr a, strip_side_effects_expr b)
  | GtExpr (a, b)            -> GtExpr (strip_side_effects_expr a, strip_side_effects_expr b)
  | NeqExpr (a, b)           -> NeqExpr (strip_side_effects_expr a, strip_side_effects_expr b)
  | EqualityExpr (a, b)      -> EqualityExpr (strip_side_effects_expr a, strip_side_effects_expr b)
  | AddressExpr a            -> AddressExpr (strip_side_effects_expr a)
  | NotExpr e                -> NotExpr (strip_side_effects_expr e)
  | ArrayAccessExpr l        -> ArrayAccessExpr (strip_side_effects_lexpr l)
  | ValueExpr                -> ValueExpr
  | SenderExpr               -> SenderExpr
  | ThisExpr                 -> ThisExpr
  | SingleDerefExpr e  -> SingleDerefExpr (strip_side_effects_expr e)
  | TupleDerefExpr  e  -> TupleDerefExpr (strip_side_effects_expr e)
  | PlusExpr (a, b)          -> PlusExpr (strip_side_effects_expr a, strip_side_effects_expr b)
  | MinusExpr (a, b)         -> MinusExpr (strip_side_effects_expr a, strip_side_effects_expr b)
  | MultExpr (a, b)          -> MultExpr (strip_side_effects_expr a, strip_side_effects_expr b)
  | BalanceExpr e            -> BalanceExpr (strip_side_effects_expr e)


and strip_side_effects_return ret =
  { ret_expr = BatOption.map strip_side_effects_expr ret.ret_expr
  ; ret_cont = strip_side_effects_expr ret.ret_cont
  }


and strip_side_effects_mthd_body (raw : (ty * 'a) mthd_body) : ty mthd_body =
  List.map strip_side_effects_stmt raw



let strip_side_effects_mthd (raw : (ty * 'a) mthd) : ty mthd =
  { mthd_head = raw.mthd_head
  ; mthd_body = strip_side_effects_mthd_body raw.mthd_body
  }


let strip_side_effects_contract (raw : (ty * 'a) contract) : ty contract =
  { contract_name = raw.contract_name
  ; contract_args = raw.contract_args
  ; mthds = List.map strip_side_effects_mthd raw.mthds
  }


let strip_side_effects (raw : (ty * 'a) Syntax.toplevel) : ty Syntax.toplevel =
  match raw with
  | Contract c  -> Contract (strip_side_effects_contract c)
  | Event e     -> Event e

let has_distinct_contract_names (contracts : unit Syntax.contract with_cid) : bool =
  let contract_names    = (List.map (fun (_, b) -> b.Syntax.contract_name) contracts) in
  List.length contracts = List.length (BatList.unique contract_names)

let assign_types (raw : unit Syntax.toplevel with_cid) :
      ty Syntax.toplevel with_cid =
  let raw_contracts : unit Syntax.contract with_cid =
    filter_map (fun x ->
                          match x with
                          | Contract c  -> Some c
                          | _           -> None
                        ) raw in
  let ()            = assert(has_distinct_contract_names(raw_contracts)) in
  let interfaces    = map Contract.contract_intf_of raw_contracts in
  let events : event with_cid =
    filter_map (fun x ->
        match x with
        | Event e   -> Some e
        | _         -> None) raw in
  map strip_side_effects (map (assign_type_toplevel interfaces events) raw)
