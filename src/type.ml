(* TODO : PEN COMPILER *) 
(* 
 * REMOVAL OF Returning Multiple Values 
 * ADDING  OF Product Types / Prouduct Terms *) 




open Printf 
open Syntax
open ContractId
open Contract
open TypeEnv
open SideEffect 

module BL   = BatList
module BS   = BatString
module BO   = BatOption
module L    = List
module Eth  = Ethereum

let err = failwith


let id_lookup_ty tenv id        = match lookup tenv id with
    | Some (tyT, Some loc)          -> IdentExpr id, (tyT, [(loc,Read)])
    | Some (tyT, None)              -> IdentExpr id, (tyT, [])
    | None                          -> err ("unknown identifier "^id)

let is_known_cntrct cn_intfs nm = BL.exists (fun(_,i)->i.cntrct_intf_name=nm) cn_intfs

let rec is_known_ty cn_intfs    = function 
    | TyRef l                       ->  BL.for_all (is_known_ty cn_intfs) l
    | TyTuple l                     ->  BL.for_all (is_known_ty cn_intfs) l
    | TyMap(a,b)                    ->  is_known_ty cn_intfs a && is_known_ty cn_intfs b
    | TyCntrctArch cntrct           ->  is_known_cntrct cn_intfs cntrct
    | TyCntrctInstance cntrct       ->  is_known_cntrct cn_intfs cntrct
    | TyUint256                     ->  true
    | TyUint8                       ->  true
    | TyBytes32                     ->  true
    | TyAddr                        ->  true
    | TyBool                        ->  true
    | TyVoid                        ->  true

let arg_has_known_ty cn_intfs arg =
    let ret = is_known_ty cn_intfs arg.ty in
    if not ret then eprintf"arg has Unknown Type %s\n"(string_of_ty arg.ty);
    ret

let ret_ty_is_known cn_intfs m  = BL.for_all (is_known_ty cn_intfs) m.mthd_ret_ty

let assignTy_mthd_head cn_intfs = function 
    | Method m              ->  assert (BL.for_all(arg_has_known_ty cn_intfs)m.mthd_args) ; 
                                assert (ret_ty_is_known cn_intfs m) ;
                                Method m
    | Default               ->  Default

let call_arg_expectations cn_intfs = function 
    | "pre_ecdsarecover"    ->  (=) [TyBytes32;TyUint8;TyBytes32;TyBytes32]
    | "keccak256"           ->  fun _ -> true
    | "iszero"              ->  fun x -> x=[TyBytes32]||x=[TyUint8]||x=[TyUint256]||x=[TyBool]||x=[TyAddr]
    | name                  ->  let cid     = lookup_id (fun c->c.cntrct_intf_name=name) cn_intfs in
                                let intf    = choose_cntrct cid cn_intfs in
                                (=) intf.cntrct_intf_args



let type_check ((expr:ty),((_,(t,_)):(ty*'a)expr)) = assert (expr = t)




let isNil x                 = x=[]
let ($) f g x               = f (g x) 
let get_eff (_,(_,eff))     = eff
let get_effs                = L.map get_eff
let get_ty  (_,(ty,_))      = ty
let get_tm  (x,_)           = x
let assert_tyeqv l r        = assert (get_ty l=get_ty r) 


let check_args_match cn_intfs (args:(ty*'x)expr list) = function 
    | Some mtd      ->  assert (call_arg_expectations cn_intfs mtd (L.map get_ty args))
    | None          ->  assert (isNil (L.map get_ty args))


let typecheck_multiple (exprs:ty list) (actual:(ty*'a)expr list) =
    L.for_all2 (fun e (_,(a,_)) -> e=a) exprs actual


let check_only_1_effect (llst:eff list list)  =
    (* write-write *)
    let ll = L.filter (BL.exists isWrite) llst in 
    if      L.length ll > 1 then err "sub-exprs have side-effects";
    (* read-write *)
    let ll'= L.filter (BL.exists isRead)  llst in  
    if      L.length ll = 0 then () 
    else if L.length ll'> 0 then err "sub-exprs have write effects and read effects"


let has_no_side_effects = isNil $ get_eff 


let rec assignTy_call cn_intfs cname tyenv (fncall:unit fn_call) : (ty*eff list)fn_call * (ty*eff list) =
    let args   = L.map (assignTy_expr cn_intfs cname tyenv) fncall.call_args in
    check_args_match cn_intfs args (Some fncall.call_head) ; 
    let effs   = get_effs args in
    check_only_1_effect effs ; 
    let effs   = (External,Write) :: L.concat effs in
    let reT    = match fncall.call_head with
        | "value" when true         -> TyUint256 (* check the arg is 'msg' *) 
        | "pre_ecdsarecover"        -> TyAddr
        | "keccak256"               -> TyBytes32
        | "iszero"                  -> begin match args with
            | [arg]                     -> TyBool
            | _                         -> err "should not happen" end 
        | cn_name when true         -> TyCntrctArch cn_name (* check contract exists *) 
        | _                         -> err "assignTy_call: should not happen" in
    ({call_head=fncall.call_head;call_args=args}, (reT,effs))


and assignTy_msg_info cn_intfs cname tyenv (msg:unit msg_info) : (ty*eff list) msg_info =
    let expr  = BO.map (assignTy_expr cn_intfs cname tyenv) msg.msg_value_info in
    let stmts = assignTy_stmts cn_intfs cname tyenv msg.msg_reentrance_info in
    { msg_value_info      = expr
    ; msg_reentrance_info = stmts }


and assignTy_expr cn_intfs cname tyenv (expr_inner,()) : (ty*eff list) expr =
    match expr_inner with
    | ThisExpr          ->  ThisExpr         , (TyCntrctInstance cname, [])
    | TrueExpr          ->  TrueExpr         , (TyBool,                 [])
    | FalseExpr         ->  FalseExpr        , (TyBool,                 [])
    | SenderExpr        ->  SenderExpr       , (TyAddr,                 [])
    | NowExpr           ->  NowExpr          , (TyUint256,              [])
    | DecLit256Expr d   ->  DecLit256Expr d  , (TyUint256,              [])
    | DecLit8Expr   d   ->  DecLit8Expr   d  , (TyUint8,                [])

    | FunCallExpr   c   ->  let c,ty = assignTy_call cn_intfs cname tyenv c in
                            FunCallExpr c    , ty

    | IdentExpr     s   ->  (* Maybe introduce a type called CallableType *)
                            if BS.starts_with s "pre_" then err "names that start with pre_ are reserved" ; 
                            id_lookup_ty tyenv s

    | ParenExpr e       ->  assignTy_expr cn_intfs cname tyenv e (* omit the parenthesis at this place*)

    | NewExpr n         ->  let n',cname'   = assignTy_new_expr cn_intfs cname tyenv n in
                            if BS.starts_with cname' "pre_" then err "names that start with pre_ are reserved"; 
                            NewExpr n'       , (TyCntrctInstance cname',[External, Write])
    | LandExpr (l, r)   ->  let l           = assignTy_expr cn_intfs cname tyenv l in
                            type_check (TyBool,l);
                            let r           = assignTy_expr cn_intfs cname tyenv r in
                            type_check (TyBool,r); 
                            let effs        = get_effs [l;r] in
                            check_only_1_effect effs; 
                            LandExpr(l,r)    , (TyBool,L.concat effs)
    | LtExpr (l, r)     ->
                            let l       = assignTy_expr cn_intfs cname tyenv l in
                            let r       = assignTy_expr cn_intfs cname tyenv r in
                            let effs    = get_effs [l; r] in
                            check_only_1_effect effs ; 
                            assert_tyeqv l r ; 
                            LtExpr(l,r)      , (TyBool,L.concat effs)
    | GtExpr (l, r)     ->
                            let l       = assignTy_expr cn_intfs cname tyenv l in
                            let r       = assignTy_expr cn_intfs cname tyenv r in
                            assert_tyeqv l r ; 
                            let effs    = get_effs [l; r] in
                            check_only_1_effect effs; 
                            GtExpr (l, r)    , (TyBool, L.concat effs)
    | NeqExpr (l, r)    ->
                            let l       = assignTy_expr cn_intfs cname tyenv l in
                            let r       = assignTy_expr cn_intfs cname tyenv r in
                            assert_tyeqv l r ;
                            let effs    = get_effs [l; r] in
                            check_only_1_effect effs; 
                            NeqExpr (l, r), (TyBool, L.concat effs)
    | EqExpr (l, r)     ->
                            let l       = assignTy_expr cn_intfs cname tyenv l in
                            let r       = assignTy_expr cn_intfs cname tyenv r in
                            assert_tyeqv l r ; 
                            let effs    = get_effs [l; r] in
                            check_only_1_effect effs;
                            EqExpr (l, r), (TyBool, L.concat effs)
    | PlusExpr (l, r)   ->
                            let l       = assignTy_expr cn_intfs cname tyenv l in
                            let r       = assignTy_expr cn_intfs cname tyenv r in
                            if snd l<>snd r 
                                then (printf "%s %s\n%!" (string_of_ty (get_ty l)) (string_of_ty (get_ty r))) ; 
                            assert_tyeqv l r ;
                            let effs    = get_effs [l; r] in
                            check_only_1_effect effs; 
                            PlusExpr (l, r), (get_ty l, L.concat effs)
    | MinusExpr (l, r)  ->
                            let l       = assignTy_expr cn_intfs cname tyenv l in
                            let r       = assignTy_expr cn_intfs cname tyenv r in
                            assert_tyeqv l r; 
                            let effs    = get_effs [l; r] in
                            check_only_1_effect effs; 
                            MinusExpr (l, r), (get_ty l, L.concat effs)
    | MultExpr (l, r)   ->
                            let l       = assignTy_expr cn_intfs cname tyenv l in
                            let r       = assignTy_expr cn_intfs cname tyenv r in
                            assert_tyeqv l r; 
                            MultExpr (l, r), snd l
    | NotExpr  e        ->
                            let e = assignTy_expr cn_intfs cname tyenv e in
                            assert (get_ty e = TyBool) ; 
                            NotExpr e       , (TyBool, get_eff e)
    | AddrExpr e        ->
                            let e = assignTy_expr cn_intfs cname tyenv e in
                            AddrExpr e      , (TyAddr, get_eff e)
    | BalanceExpr e     ->
                            let e = assignTy_expr cn_intfs cname tyenv e in
                            assert (acceptable_as TyAddr (get_ty e));
                            assert (get_eff e = []);
                            BalanceExpr e   , (TyUint256, [External, Read])
    | ArrayAccessExpr aa->
                            let e = assignTy_expr cn_intfs cname tyenv (read_array_access aa).array_access_array in
                            begin match get_ty e with
                            | TyMap(kT,vT)-> let aaidx              = (read_array_access aa).array_access_index in 
                                             let idx,(idxTy,idxEff) = assignTy_expr cn_intfs cname tyenv aaidx in 
                                             assert (acceptable_as kT idxTy) ; 
                                             assert (BL.for_all isStorRead idxEff) ; 
                                             (* TODO Check idxTy and key_ty are somehow compatible *)
                                             (ArrayAccessExpr (ArrayAccessLExpr
                                                      { array_access_array = e
                                                      ; array_access_index = idx,(idxTy,idxEff) }),(vT,[Stor,Read]))
                            | _           -> err "index access has to be on mappings"   end
    | SendExpr send     ->
                            let msg_info'   = assignTy_msg_info cn_intfs cname tyenv send.send_msg_info in
                            let cntrct'     = assignTy_expr cn_intfs cname tyenv send.send_cntrct in
                            begin match send.send_mthd with
                            | Some m ->
                                let cntrct_name = cntrct_name_of_instance cntrct' in
                                let method_sig : Eth.fn_sig = begin
                                    match find_mthd_sig cn_intfs cntrct_name m with
                                    | Some x -> x
                                    | None   -> err ("method "^m^" not found") end in
                                let types   = Eth.(L.map to_ty (method_sig.sig_return)) in
                                let args    = L.map (assignTy_expr cn_intfs cname tyenv) send.send_args in
                                assert (BL.for_all has_no_side_effects args) ;
                                let reference = SendExpr{ send_cntrct   = cntrct'
                                                        ; send_mthd   = send.send_mthd
                                                        ; send_args     = args
                                                        ; send_msg_info = msg_info' }, (TyRef types,[External,Write])  in
                                (match types with
                                 | [single] -> (SingleDerefExpr reference, (single, [External, Write]))
                                 | _        -> reference)
                            | None ->
                               let () = assert (send.send_args = []) in
                               ( SendExpr
                                   { send_cntrct = cntrct'
                                   ; send_mthd = None
                                   ; send_args = []
                                   ; send_msg_info = msg_info'
                                   }, (TyVoid, [External, Write]) )
                            end
    | ValueExpr         ->  ValueExpr, (TyUint256,[])
    | SingleDerefExpr _
    | TupleDerefExpr _  ->  err "DerefExpr not supposed to appear in the raw tree for now"


and assignTy_new_expr cn_intfs cname tenv e : (ty*eff list)new_expr*string (* name of the cntrct just created *) =
    let msg_info'   =   assignTy_msg_info cn_intfs cname tenv e.new_msg_info in
    let args'       =   L.map (assignTy_expr cn_intfs cname tenv) e.new_args in
    let e'          =   { new_head      = e.new_head
                        ; new_args      = args'
                        ; new_msg_info  = msg_info' } in
    (e', e.new_head )

  
and assignTy_lexpr cn_intfs cname tyenv (src:unit lexpr) : (ty*eff list) lexpr =
  (* no need to type the left hand side? *)
    match src with
    | ArrayAccessLExpr aa ->let e = assignTy_expr cn_intfs cname tyenv aa.array_access_array in
                            begin match get_ty e with
                            | TyMap (kT,vT) ->
                               let idx,idx_ty = assignTy_expr cn_intfs cname tyenv aa.array_access_index in
                               (* TODO Check idx_typ' and key_ty are somehow compatible *)
                               ArrayAccessLExpr { array_access_array = e 
                                                ; array_access_index = idx,idx_ty  }
                            | _             -> err ("unknown array") end


and assignTy_return cn_intfs cname tyenv (ret:unit return) : (ty*eff list) return =
    let ret_ty_expr = BO.map (assignTy_expr cn_intfs cname tyenv) ret.ret_expr in
    let retTyCheck  = lookup_retTyCheck tyenv in
    assert (retTyCheck(BO.map get_ty ret_ty_expr)); 
    { ret_expr  = ret_ty_expr
    ; ret_cont  = assignTy_expr cn_intfs cname tyenv ret.ret_cont }


and assignTy_varDecl cn_intfs cname tyenv (vd:unit varDecl): (ty*eff list)varDecl * ty_env =
    let v       = assignTy_expr cn_intfs cname tyenv vd.varDecl_val in
    let id      = vd.varDecl_id     in
    let ty      = vd.varDecl_ty     in
    if BS.starts_with id "pre_" then err "Names \"pre_..\" are reserved" ;
    assert (is_known_ty cn_intfs ty);
    let tyenv'  = add_pair tyenv id ty None in
    let vd'     =   { varDecl_ty    = ty
                    ; varDecl_id    = id
                    ; varDecl_val   = v  } in
    vd', tyenv'


and assignTy_stmt cn_intfs cname tyenv (src:unit stmt) : ((ty*eff list)stmt * ty_env) =
    match src with
    | AbortStmt         ->  AbortStmt, tyenv
    | ReturnStmt r      ->
                            let r = assignTy_return cn_intfs cname tyenv r in
                            ReturnStmt r, tyenv
    | AssignStmt(l,r)   ->
                            let l = assignTy_lexpr cn_intfs cname tyenv l in
                            let r = assignTy_expr  cn_intfs cname tyenv r in
                            AssignStmt(l,r), tyenv
    | IfThenOnly(b,t)   ->
                            let b = assignTy_expr  cn_intfs cname tyenv b in
                            let t = assignTy_stmts cn_intfs cname tyenv t in
                            IfThenOnly(b,t), tyenv
    | IfThenElse(b,t,f) ->
                            let b = assignTy_expr  cn_intfs cname tyenv b in
                            let t = assignTy_stmts cn_intfs cname tyenv t in
                            let f = assignTy_stmts cn_intfs cname tyenv f in
                            IfThenElse(b,t,f), tyenv
    | SelfDestructStmt e ->
                            let e = assignTy_expr  cn_intfs cname tyenv e in
                            SelfDestructStmt e, tyenv
    | VarDeclStmt vd    ->
                            let vd,tyenv = assignTy_varDecl cn_intfs cname tyenv vd in
                            VarDeclStmt vd, tyenv
    | ExprStmt e        ->
                            let e = assignTy_expr  cn_intfs cname tyenv e in
                            assert(get_ty e = TyVoid) ;
                            assert(BL.exists isWrite (get_eff e)) ; 
                            ExprStmt e, tyenv
    | LogStmt(nm,args,_)->
                            let args    = L.map(assignTy_expr cn_intfs cname tyenv)args in
                            let ev      = lookup_event tyenv nm in
                            let tys     = L.map(fun ea->ea.event_arg_body.ty)ev.event_args in
                            assert (typecheck_multiple tys args) ;
                            let effs    = get_effs args in
                            check_only_1_effect effs ; 
                            LogStmt(nm,args,Some ev), tyenv


and assignTy_stmts cn_intfs cname tyenv = function (* unit stmt list -> (ty*eff list)stmt list*) 
    | []            ->  []
    | stmt::rest    ->  let stmt,tyenv = assignTy_stmt cn_intfs cname tyenv stmt in
                        stmt :: assignTy_stmts cn_intfs cname tyenv rest


(* Termination *) 

type termination    =
                    | OnTheWay 
                    | ReturnValues of int 
                    | JustStop

let rec is_terminating = function 
    | AbortStmt                 -> [JustStop]
    | SelfDestructStmt _        -> [JustStop]
    | ReturnStmt ret            -> begin match ret.ret_expr with
        | Some _                    -> [ReturnValues 1]
        | None                      -> [ReturnValues 0] end
    | IfThenOnly(_,b)           -> (are_terminating b) @ [OnTheWay] (* there is a continuation if the condition does not hold. *)
    | IfThenElse(_,bT,bF)       -> are_terminating bT @ (are_terminating bF)
    | AssignStmt _              -> [OnTheWay]
    | VarDeclStmt _             -> [OnTheWay]
    | ExprStmt _                -> [OnTheWay]
    | LogStmt _                 -> [OnTheWay]

and are_terminating stmts =
  let last_stmt = BL.last stmts in
  is_terminating last_stmt




  
(* AssignTy Method / Contract / Toplevel  *) 

(* Default Method Returns Void is a specification *) 

let mthd_is_returning_void (mthd : unit mthd) = match mthd.mthd_head with
    | Default       ->  true
    | Method m      ->  m.mthd_ret_ty = []

let retTyCheck_of_mthd m ty_inferred = match m, ty_inferred with
    | Default ,Some _    ->  false
    | Default ,None      ->  true
    | Method u,   _      ->  begin match u.mthd_ret_ty, ty_inferred with
        | _::_::_ , _       -> false
        | [x], Some y       -> acceptable_as x y
        | [] , None         -> true
        | _, _              -> false  end


let assignTy_mthd cn_intfs cn_name tenv (m : unit mthd) =
    assert (L.for_all (function 
                         | OnTheWay         -> false
                         | ReturnValues 0   -> mthd_is_returning_void m
                         | ReturnValues 1   -> not (mthd_is_returning_void m)
                         | ReturnValues _   -> err "multiple vals return not supported yry"
                         | JustStop         -> true )  (are_terminating m.mthd_body)) ; 
    let margs       = mthd_head_arg_list m.mthd_head in
    if BL.exists (fun arg -> BS.starts_with arg.id "pre_") margs 
                then err "names that start with pre_ are reserved"; 
    let retTyCheck  = retTyCheck_of_mthd m.mthd_head in
    let tyenv'      = set_retTyCheck (add_block margs tenv) retTyCheck in 
    { mthd_head = assignTy_mthd_head cn_intfs m.mthd_head
    ; mthd_body = assignTy_stmts cn_intfs cn_name tyenv' m.mthd_body }

let has_distinct_sigs (cn:unit cntrct) =
    let mthds       =   cn.mthds in
    let sigs        =   L.map (fun m -> match m.mthd_head with
                              | Method m' -> Some (Eth.string_of_mthd_info_ty m')
                              | Default   -> None) mthds in
    let uniq_sigs   =   BL.unique sigs in
    L.length sigs=L.length uniq_sigs


let assignTy_cntrct cn_intfs (evs: event with_cid)(cn : unit cntrct) 
            : (ty*eff list) cntrct =
    assert (BL.for_all (arg_has_known_ty cn_intfs) cn.cntrct_args) ; 
    assert (has_distinct_sigs cn);
    let tyenv  = add_block cn.cntrct_args (add_events evs empty_ty_env) in
    if BS.starts_with cn.cntrct_name "pre_" 
        then err "names \"pre_..\" are reserved" ; 
    if BL.exists (fun arg -> BS.starts_with arg.id "pre_") cn.cntrct_args 
        then err "names \"pre_..\" are reserved" ; 
    { cntrct_name     = cn.cntrct_name
    ; cntrct_args     = cn.cntrct_args
    ; mthds           = L.map(assignTy_mthd cn_intfs cn.cntrct_name tyenv)cn.mthds }


let assignTy_toplevel cn_intfs (evs:event with_cid) (top:unit toplevel) : (ty*eff list)toplevel 
    = match top with
    | Cntrct c    -> Cntrct (assignTy_cntrct cn_intfs evs c)
    | Event  e    -> Event e








(**********************)   
(* Strip Side Effects *) 
(**********************)   

(* XXX: these [stripEffect_X] should be generalized over any f : 'a -> 'b *)

let rec stripEffect_stmt (raw:(ty*'a)stmt): ty stmt =
    match raw with
    | AbortStmt               -> AbortStmt
    | ReturnStmt ret          -> ReturnStmt (stripEffect_return ret)
    | AssignStmt(l,r)         -> AssignStmt (stripEffect_lexpr l, stripEffect_expr r)
    | VarDeclStmt v           -> VarDeclStmt(stripEffect_varDecl v)
    | IfThenOnly(b,blk)       -> IfThenOnly (stripEffect_expr b, stripEffect_mthd_body blk)
    | IfThenElse(b,t,u)       -> IfThenElse (stripEffect_expr b,stripEffect_mthd_body t,stripEffect_mthd_body u)
    | SelfDestructStmt e      -> SelfDestructStmt (stripEffect_expr e)
    | ExprStmt e              -> ExprStmt   (stripEffect_expr e)
    | LogStmt(str,args,eopt)  -> LogStmt (str, L.map stripEffect_expr args, eopt)

and stripEffect_varDecl v =
    { varDecl_ty           = v.varDecl_ty
    ; varDecl_id         = v.varDecl_id
    ; varDecl_val        = stripEffect_expr v.varDecl_val }

and stripEffect_aa aa =
    { array_access_array    = stripEffect_expr aa.array_access_array
    ; array_access_index    = stripEffect_expr aa.array_access_index }

and stripEffect_lexpr = function 
    | ArrayAccessLExpr aa          -> ArrayAccessLExpr (stripEffect_aa aa)

and stripEffect_expr(i,(t,_)) = stripEffect_expr_inner i, t

and stripEffect_fn_call fc =
    { call_head             = fc.call_head
    ; call_args             = L.map stripEffect_expr fc.call_args }

and stripEffect_msg_info m =
    { msg_value_info        = BO.map stripEffect_expr m.msg_value_info
    ; msg_reentrance_info   = L.map stripEffect_stmt m.msg_reentrance_info  }

and stripEffect_send s =
    { send_cntrct           = stripEffect_expr s.send_cntrct
    ; send_mthd           = s.send_mthd
    ; send_args             = L.map stripEffect_expr s.send_args
    ; send_msg_info         = stripEffect_msg_info s.send_msg_info }

and stripEffect_new_expr n =
    { new_head              = n.new_head
    ; new_args              = L.map stripEffect_expr n.new_args
    ; new_msg_info          = stripEffect_msg_info n.new_msg_info }

and stripEffect_expr_inner = function 
    | TrueExpr              -> TrueExpr
    | FalseExpr             -> FalseExpr
    | DecLit256Expr d       -> DecLit256Expr d
    | DecLit8Expr d         -> DecLit8Expr d
    | NowExpr               -> NowExpr
    | FunCallExpr fc        -> FunCallExpr (stripEffect_fn_call fc)
    | IdentExpr str         -> IdentExpr str
    | ParenExpr e         -> ParenExpr (stripEffect_expr e)
    | NewExpr e             -> NewExpr (stripEffect_new_expr e)
    | SendExpr send         -> SendExpr (stripEffect_send send)
    | LandExpr(a,b)         -> LandExpr (stripEffect_expr a, stripEffect_expr b)
    | LtExpr(a,b)           -> LtExpr (stripEffect_expr a, stripEffect_expr b)
    | GtExpr(a,b)           -> GtExpr (stripEffect_expr a, stripEffect_expr b)
    | NeqExpr(a,b)          -> NeqExpr (stripEffect_expr a, stripEffect_expr b)
    | EqExpr(a,b)           -> EqExpr (stripEffect_expr a, stripEffect_expr b)
    | AddrExpr a            -> AddrExpr (stripEffect_expr a)
    | NotExpr e             -> NotExpr (stripEffect_expr e)
    | ArrayAccessExpr l     -> ArrayAccessExpr (stripEffect_lexpr l)
    | ValueExpr             -> ValueExpr
    | SenderExpr            -> SenderExpr
    | ThisExpr              -> ThisExpr
    | SingleDerefExpr e     -> SingleDerefExpr (stripEffect_expr e)
    | TupleDerefExpr  e     -> TupleDerefExpr (stripEffect_expr e)
    | PlusExpr(a,b)         -> PlusExpr (stripEffect_expr a, stripEffect_expr b)
    | MinusExpr(a,b)        -> MinusExpr (stripEffect_expr a, stripEffect_expr b)
    | MultExpr(a,b)         -> MultExpr (stripEffect_expr a, stripEffect_expr b)
    | BalanceExpr e         -> BalanceExpr (stripEffect_expr e)

and stripEffect_return ret =
    { ret_expr = BO.map stripEffect_expr ret.ret_expr
    ; ret_cont = stripEffect_expr ret.ret_cont           }

and stripEffect_mthd_body (raw:(ty*'a)mthd_body) : ty mthd_body =
    L.map stripEffect_stmt raw

let stripEffect_mthd (raw:(ty*'a) mthd) : ty mthd =
    { mthd_head = raw.mthd_head
    ; mthd_body = stripEffect_mthd_body raw.mthd_body }

let stripEffect_cntrct (raw:(ty*'a)cntrct) : ty cntrct =
    { cntrct_name = raw.cntrct_name
    ; cntrct_args = raw.cntrct_args
    ; mthds       = L.map stripEffect_mthd raw.mthds }

let stripEffect (raw:(ty*'a)toplevel) : ty toplevel =
    match raw with
    | Cntrct c      -> Cntrct (stripEffect_cntrct c)
    | Event e       -> Event e







(* Assign Type *) 

let has_distinct_cntrct_names (cns : unit cntrct with_cid) : bool =
    let cn_names    = (L.map (fun (_, b) -> b.cntrct_name) cns) in
    L.length cns=L.length(BL.unique cn_names)


let assignTys (raw : unit toplevel with_cid) : ty toplevel with_cid =
    let raw_cntrcts : unit cntrct with_cid =
      filter_map (function  | Cntrct c -> Some c
                            | _        -> None   ) raw in
    assert(has_distinct_cntrct_names(raw_cntrcts));
    let intfs = map cntrct_intf_of raw_cntrcts in
    let evs : event with_cid = filter_map (function | Event e -> Some e
                                                    | _       -> None   ) raw in
    map stripEffect (map (assignTy_toplevel intfs evs) raw)
