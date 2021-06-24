(* TODO : PEN COMPILER *) 
(* 
 * REMOVAL OF Returning Multiple Values 
 * ADDING  OF Product Types / Prouduct Terms *) 




open Printf 

open Misc 
open Syntax
open IndexedList
open Contract
open TypeEnv
open TypeEffect 

module BL   = BatList
module BS   = BatString
module BO   = BatOption
module L    = List
module Eth  = Ethereum



let id_lookup_ty tenv id        =   match lookup tenv id with
    | Some (tyT, Some loc)          -> EpIdent id, (tyT, [(loc,Read)])
    | Some (tyT, None)              -> EpIdent id, (tyT, [])
    | None                          -> err ("unknown identifier "^id)

let is_known_cntrct tyCns nm    =   BL.exists (fun(_,i)->i.tyCntrct_name=nm) tyCns

let rec is_known_ty tyCns       =   function 
    | TyRef l                       ->  is_known_ty tyCns l
    | TyTuple l                     ->  BL.for_all (is_known_ty tyCns) l
    | TyMap(a,b)                    ->  is_known_ty tyCns a && is_known_ty tyCns b
    | TyCntrct cn                   ->  is_known_cntrct tyCns cn
    | TyInstnce cn                  ->  is_known_cntrct tyCns cn
    | TyUint256                     ->  true
    | TyUint8                       ->  true
    | TyBytes32                     ->  true
    | TyAddr                        ->  true
    | TyBool                        ->  true
    | TyVoid                        ->  true

let arg_has_known_ty tyCns arg  =   let ret = is_known_ty tyCns arg.ty in
                                        if not ret then eprintf"arg has Unknown Type %s\n"(string_of_ty arg.ty);
                                        ret

let retTy_is_known tyCns m      =   is_known_ty tyCns m.mthd_retTy

let assignTy_mthd_head tyCns    =   function 
    | Method m                      ->  assert (BL.for_all(arg_has_known_ty tyCns)m.mthd_args) ; 
                                        assert (retTy_is_known tyCns m) ;
                                        Method m
    | Default                       ->  Default

let call_arg_expectations tyCns =   function 
    | "pre_ecdsarecover"            ->  (=) [TyBytes32;TyUint8;TyBytes32;TyBytes32]
    | "keccak256"                   ->  fun _ -> true
    | "iszero"                      ->  fun x -> x=[TyBytes32]||x=[TyUint8]||x=[TyUint256]||x=[TyBool]||x=[TyAddr]
    | name                          ->  let idx     = lookup_idx (fun c->c.tyCntrct_name=name) tyCns in
                                        let tyCn    = lookup_index idx tyCns in
                                        (=) tyCn.tyCntrct_args



let type_check ((expr:ty),((_,(t,_)):(ty*'a)expr)) = assert (expr = t)




let get_eff (_,(_,eff))     = eff
let get_effs                = L.map get_eff
let get_ty  (_,(ty,_))      = ty
let get_tm  (x,_)           = x
let assert_tyeqv l r        = assert (get_ty l=get_ty r) 


let check_args_match tyCns (args:(ty*'x)expr list) = function 
    | Some m        ->  assert (call_arg_expectations tyCns m (L.map get_ty args))
    | None          ->  assert (isNil (L.map get_ty args))


let typecheck_multiple (exprs:ty list) (actual:(ty*'a)expr list) =
    L.for_all2 (fun e (_,(a,_))-> e=a) exprs actual


let check_only_1_effect (llst:eff list list)  =
    (* write-write *)
    let ll = L.filter (BL.exists isWrite) llst in 
    if      L.length ll > 1 then err "sub-exprs have side-effects";
    (* read-write *)
    let ll'= L.filter (BL.exists isRead)  llst in  
    if      L.length ll = 0 then () 
    else if L.length ll'> 0 then err "sub-exprs have write effects and read effects"


let has_no_side_effects = isNil $ get_eff 


let rec assignTy_call tyCns cname tyenv (fncall:unit fncall) : (ty*eff list)fncall * (ty*eff list) =
    let args   = L.map (assignTy_expr tyCns cname tyenv) fncall.call_args in
    check_args_match tyCns args (Some fncall.call_head) ; 
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
        | cn_name when true         -> TyCntrct cn_name (* check contract exists *) 
        | _                         -> err "assignTy_call: should not happen" in
    ({call_head=fncall.call_head;call_args=args}, (reT,effs))


and assignTy_msg tyCns cname tyenv (msg:unit msg) : (ty*eff list) msg =
    let expr  = BO.map (assignTy_expr tyCns cname tyenv) msg.msg_value in
    let stmts = assignTy_stmts tyCns cname tyenv msg.msg_reentrance in
    { msg_value      = expr
    ; msg_reentrance = stmts }


and assignTy_expr tyCns cname tyenv (expr_inner,()) : (ty*eff list) expr =
    match expr_inner with
    | EpThis          ->    EpThis         , (TyInstnce cname, [])
    | EpTrue          ->    EpTrue         , (TyBool,                 [])
    | EpFalse         ->    EpFalse        , (TyBool,                 [])
    | EpSender        ->    EpSender       , (TyAddr,                 [])
    | EpNow           ->    EpNow          , (TyUint256,              [])
    | EpDecLit256 d   ->    EpDecLit256 d  , (TyUint256,              [])
    | EpDecLit8   d   ->    EpDecLit8   d  , (TyUint8,                [])

    | EpFnCall   c    ->    let c,ty = assignTy_call tyCns cname tyenv c in
                            EpFnCall c    , ty

    | EpIdent     s   ->    (* Maybe introduce a type called CallableType *)
                            if BS.starts_with s "pre_" then err "names that start with pre_ are reserved" ; 
                            id_lookup_ty tyenv s

    | EpParen e       ->    assignTy_expr tyCns cname tyenv e (* omit the parenthesis at this place*)

    | EpNew n         ->    let n',cname'   = assignTy_new_expr tyCns cname tyenv n in
                            if BS.starts_with cname' "pre_" then err "names that start with pre_ are reserved"; 
                            EpNew n'       , (TyInstnce cname',[External, Write])
    | EpLand (l, r)   ->    let l           = assignTy_expr tyCns cname tyenv l in
                            type_check (TyBool,l);
                            let r           = assignTy_expr tyCns cname tyenv r in
                            type_check (TyBool,r); 
                            let effs        = get_effs [l;r] in
                            check_only_1_effect effs; 
                            EpLand(l,r)    , (TyBool,L.concat effs)
    | EpLt (l, r)     ->
                            let l       = assignTy_expr tyCns cname tyenv l in
                            let r       = assignTy_expr tyCns cname tyenv r in
                            let effs    = get_effs [l; r] in
                            check_only_1_effect effs ; 
                            assert_tyeqv l r ; 
                            EpLt(l,r)      , (TyBool,L.concat effs)
    | EpGt (l, r)     ->
                            let l       = assignTy_expr tyCns cname tyenv l in
                            let r       = assignTy_expr tyCns cname tyenv r in
                            assert_tyeqv l r ; 
                            let effs    = get_effs [l; r] in
                            check_only_1_effect effs; 
                            EpGt (l, r)    , (TyBool, L.concat effs)
    | EpNeq (l, r)    ->
                            let l       = assignTy_expr tyCns cname tyenv l in
                            let r       = assignTy_expr tyCns cname tyenv r in
                            assert_tyeqv l r ;
                            let effs    = get_effs [l; r] in
                            check_only_1_effect effs; 
                            EpNeq (l, r), (TyBool, L.concat effs)
    | EpEq (l, r)     ->
                            let l       = assignTy_expr tyCns cname tyenv l in
                            let r       = assignTy_expr tyCns cname tyenv r in
                            assert_tyeqv l r ; 
                            let effs    = get_effs [l; r] in
                            check_only_1_effect effs;
                            EpEq (l, r), (TyBool, L.concat effs)
    | EpPlus (l, r)   ->
                            let l       = assignTy_expr tyCns cname tyenv l in
                            let r       = assignTy_expr tyCns cname tyenv r in
                            assert_tyeqv l r ;
                            let effs    = get_effs [l; r] in
                            check_only_1_effect effs; 
                            EpPlus (l, r), (get_ty l, L.concat effs)
    | EpMinus (l, r)  ->
                            let l       = assignTy_expr tyCns cname tyenv l in
                            let r       = assignTy_expr tyCns cname tyenv r in
                            assert_tyeqv l r; 
                            let effs    = get_effs [l; r] in
                            check_only_1_effect effs; 
                            EpMinus (l, r), (get_ty l, L.concat effs)
    | EpMult (l, r)   ->
                            let l       = assignTy_expr tyCns cname tyenv l in
                            let r       = assignTy_expr tyCns cname tyenv r in
                            assert_tyeqv l r; 
                            EpMult (l, r), snd l
    | EpNot  e        ->
                            let e = assignTy_expr tyCns cname tyenv e in
                            assert (get_ty e = TyBool) ; 
                            EpNot e       , (TyBool, get_eff e)
    | EpAddr e        ->
                            let e = assignTy_expr tyCns cname tyenv e in
                            EpAddr e      , (TyAddr, get_eff e)
    | EpBalance e     ->
                            let e = assignTy_expr tyCns cname tyenv e in
                            assert (acceptable_as TyAddr (get_ty e));
                            assert (get_eff e = []);
                            EpBalance e   , (TyUint256, [External, Read])
    | EpArray aa->
                            let e = assignTy_expr tyCns cname tyenv (read_array aa).array_name in
                            begin match get_ty e with
                            | TyMap(kT,vT)-> let aaidx              = (read_array aa).array_index in 
                                             let idx,(idxTy,idxEff) = assignTy_expr tyCns cname tyenv aaidx in 
                                             assert (acceptable_as kT idxTy) ; 
                                             assert (BL.for_all isStorRead idxEff) ; 
                                             (* TODO Check idxTy and key_ty are somehow compatible *)
                                             (EpArray (LEpArray
                                                      { array_name = e
                                                      ; array_index = idx,(idxTy,idxEff) }),(vT,[Stor,Read]))
                            | _           -> err "index access has to be on mappings"   end
    | EpSend send     ->
                            let msg         = assignTy_msg tyCns cname tyenv send.send_msg in
                            let cn'         = assignTy_expr tyCns cname tyenv send.send_cntrct in
                            begin match send.send_mthd with
                            | Some m ->
                                        let tyMthd  =   begin match find_tyMthd tyCns m with
                                                        | Some ty   -> ty
                                                        | None      -> err("method "^m^" not found") end in
                                        let tyRet   =   tyMthd.tyRet in
                                        let args    =   L.map (assignTy_expr tyCns cname tyenv) send.send_args in
                                        assert (BL.for_all has_no_side_effects args) ;
                                        let ref     =   EpSend  { send_cntrct   = cn'
                                                                ; send_mthd     = send.send_mthd
                                                                ; send_args     = args
                                                                ; send_msg      = msg           }, 
                                                                (TyRef tyRet,[External,Write])          in
                                        (match tyRet with
                                         | TyVoid   -> ref 
                                         | ty       -> EpSingleDeref ref, (ty, [External, Write]) )
                            | None ->
                                        assert (send.send_args = []) ; 
                                        EpSend { send_cntrct    = cn'
                                               ; send_mthd      = None
                                               ; send_args      = []
                                               ; send_msg  = msg }, (TyVoid, [External, Write]) end
    | EpValue         ->    EpValue, (TyUint256,[])
    | EpSingleDeref _
    | EpTupleDeref  _ ->    err "DerefEp not supposed (Bamboo Bad Designing)"


and assignTy_new_expr tyCns cname tenv e : (ty*eff list)new_expr*string (* name of the cntrct just created *) =
    let msg'        =   assignTy_msg tyCns cname tenv e.new_msg in
    let args'       =   L.map (assignTy_expr tyCns cname tenv) e.new_args in
    let e'          =   { new_head      = e.new_head
                        ; new_args      = args'
                        ; new_msg  = msg' } in
    (e', e.new_head )

  
and assignTy_lexpr tyCns cname tyenv (src:unit lexpr) : (ty*eff list) lexpr =
  (* no need to type the left hand side? *)
    match src with
    | LEpArray aa ->let e = assignTy_expr tyCns cname tyenv aa.array_name in
                            begin match get_ty e with
                            | TyMap (kT,vT) ->
                               let idx,idx_ty = assignTy_expr tyCns cname tyenv aa.array_index in
                               (* TODO Check idx_typ' and key_ty are somehow compatible *)
                               LEpArray { array_name = e 
                                                ; array_index = idx,idx_ty  }
                            | _             -> err ("unknown array") end


and assignTy_return tyCns cname tyenv (ret:unit return) : (ty*eff list) return =
    let retTy_expr = BO.map (assignTy_expr tyCns cname tyenv) ret.ret_expr in
    let retTyCheck  = lookup_retTyCheck tyenv in
    assert (retTyCheck(BO.map get_ty retTy_expr)); 
    { ret_expr  = retTy_expr
    ; ret_cont  = assignTy_expr tyCns cname tyenv ret.ret_cont }


and assignTy_varDecl tyCns cname tyenv (vd:unit varDecl): (ty*eff list)varDecl * tyEnv =
    let v       = assignTy_expr tyCns cname tyenv vd.varDecl_val in
    let id      = vd.varDecl_id     in
    let ty      = vd.varDecl_ty     in
    if BS.starts_with id "pre_" then err "Names \"pre_..\" are reserved" ;
    assert (is_known_ty tyCns ty);
    let tyenv'  = add_var tyenv id ty None in
    let vd'     =   { varDecl_ty    = ty
                    ; varDecl_id    = id
                    ; varDecl_val   = v  } in
    vd', tyenv'


and assignTy_stmt tyCns cname tyenv (src:unit stmt) : ((ty*eff list)stmt * tyEnv) =
    match src with
    | SmAbort         ->    SmAbort, tyenv
    | SmReturn r      ->    let r = assignTy_return tyCns cname tyenv r in
                            SmReturn r, tyenv
    | SmAssign(l,r)   ->    let l = assignTy_lexpr tyCns cname tyenv l in
                            let r = assignTy_expr  tyCns cname tyenv r in
                            SmAssign(l,r), tyenv
    | SmIfThen(b,t)   ->    let b = assignTy_expr  tyCns cname tyenv b in
                            let t = assignTy_stmts tyCns cname tyenv t in
                            SmIfThen(b,t), tyenv
    | SmIfThenElse(b,t,f) ->
                            let b = assignTy_expr  tyCns cname tyenv b in
                            let t = assignTy_stmts tyCns cname tyenv t in
                            let f = assignTy_stmts tyCns cname tyenv f in
                            SmIfThenElse(b,t,f), tyenv
    | SmSelfDestruct e ->
                            let e = assignTy_expr  tyCns cname tyenv e in
                            SmSelfDestruct e, tyenv
    | SmVarDecl vd    ->
                            let vd,tyenv = assignTy_varDecl tyCns cname tyenv vd in
                            SmVarDecl vd, tyenv
    | SmExpr e        ->
                            let e = assignTy_expr  tyCns cname tyenv e in
                            assert(get_ty e = TyVoid) ;
                            assert(BL.exists isWrite (get_eff e)) ; 
                            SmExpr e, tyenv
    | SmLog(nm,args,_)->
                            let args    = L.map(assignTy_expr tyCns cname tyenv)args in
                            let ev      = lookup_evnt tyenv nm in
                            let tys     = L.map(fun ea->ea.arg.ty)ev.evnt_args in
                            assert (typecheck_multiple tys args) ;
                            let effs    = get_effs args in
                            check_only_1_effect effs ; 
                            SmLog(nm,args,Some ev), tyenv


and assignTy_stmts tyCns cname tyenv = function (* unit stmt list -> (ty*eff list)stmt list*) 
    | []            ->  []
    | stmt::rest    ->  let stmt,tyenv = assignTy_stmt tyCns cname tyenv stmt in
                        stmt :: assignTy_stmts tyCns cname tyenv rest


(* Termination *) 

type termination            =
                            | OnTheWay 
                            | ReturnValues of int 
                            | JustStop

let rec is_terminating = function 
    | SmAbort                 -> [JustStop]
    | SmSelfDestruct _        -> [JustStop]
    | SmReturn ret            -> begin match ret.ret_expr with
        | Some _                    -> [ReturnValues 1]
        | None                      -> [ReturnValues 0] end
    | SmIfThen(_,b)           -> (are_terminating b) @ [OnTheWay] (* there is a continuation if the condition does not hold. *)
    | SmIfThenElse(_,bT,bF)       -> are_terminating bT @ (are_terminating bF)
    | SmAssign _              -> [OnTheWay]
    | SmVarDecl _             -> [OnTheWay]
    | SmExpr _                -> [OnTheWay]
    | SmLog _                 -> [OnTheWay]

and are_terminating stmts =
  let last_stmt = BL.last stmts in
  is_terminating last_stmt




  
(* AssignTy Method / Contract / Toplevel  *) 

(* Default Method Returns Void is a specification *) 

let mthd_is_returning_void (mthd : unit mthd) = match mthd.mthd_head with
    | Default       ->  true
    | Method m      ->  m.mthd_retTy = TyVoid

let retTyCheck_of_mthd m ty_inferred = match m, ty_inferred with
    | Default ,Some _    ->  false
    | Default ,None      ->  true
    | Method u,   _      ->  begin match u.mthd_retTy, ty_inferred with
        | tyVoid, None      -> true
        | x, Some y         -> acceptable_as x y
        | _, _              -> false  end


let assignTy_mthd tyCns cn_name tenv (m : unit mthd) =
    assert (L.for_all (function 
                         | OnTheWay         -> false
                         | ReturnValues 0   -> mthd_is_returning_void m
                         | ReturnValues 1   -> not (mthd_is_returning_void m)
                         | ReturnValues _   -> err "multiple vals return not supported yry"
                         | JustStop         -> true )  (are_terminating m.mthd_body)) ; 
    let margs       = args_of_mthd m.mthd_head in
    if BL.exists (fun arg -> BS.starts_with arg.id "pre_") margs 
                then err "names that start with pre_ are reserved"; 
    let retTyCheck  = retTyCheck_of_mthd m.mthd_head in
    let tyenv'      = set_retTyCheck (add_block margs tenv) retTyCheck in 
    { mthd_head = assignTy_mthd_head tyCns m.mthd_head
    ; mthd_body = assignTy_stmts tyCns cn_name tyenv' m.mthd_body }

let has_distinct_sigs (cn:unit cntrct) =
    let mthds       =   cn.mthds in
    let sigs        =   L.map (fun m -> match m.mthd_head with
                              | Method m' -> Some (Eth.string_of_tyMthd m')
                              | Default   -> None) mthds in
    let uniq_sigs   =   BL.unique sigs in
    L.length sigs=L.length uniq_sigs


let assignTy_cntrct tyCns (evs: evnt idx_list)(cn : unit cntrct) 
            : (ty*eff list) cntrct =
    assert (BL.for_all (arg_has_known_ty tyCns) cn.cntrct_args) ; 
    assert (has_distinct_sigs cn);
    let tyenv  = add_block cn.cntrct_args (add_evnts evs empty_tyEnv) in
    if BS.starts_with cn.cntrct_name "pre_" 
        then err "names \"pre_..\" are reserved" ; 
    if BL.exists (fun arg -> BS.starts_with arg.id "pre_") cn.cntrct_args 
        then err "names \"pre_..\" are reserved" ; 
    { cntrct_name     = cn.cntrct_name
    ; cntrct_args     = cn.cntrct_args
    ; mthds           = L.map(assignTy_mthd tyCns cn.cntrct_name tyenv)cn.mthds }


let assignTy_toplevel tyCns (evs:evnt idx_list) (top:unit toplevel) : (ty*eff list)toplevel 
    = match top with
    | Cntrct c    -> Cntrct (assignTy_cntrct tyCns evs c)
    | Event  e    -> Event e








(**********************)   
(* Strip Side Effects *) 
(**********************)   

(* XXX: these [stripEffect_X] should be generalized over any f : 'a -> 'b *)

let rec stripEffect_stmt (raw:(ty*'a)stmt): ty stmt =
    match raw with
    | SmAbort               -> SmAbort
    | SmReturn ret          -> SmReturn (stripEffect_return ret)
    | SmAssign(l,r)         -> SmAssign (stripEffect_lexpr l, stripEffect_expr r)
    | SmVarDecl v           -> SmVarDecl(stripEffect_varDecl v)
    | SmIfThen(b,blk)       -> SmIfThen (stripEffect_expr b, stripEffect_mthd_body blk)
    | SmIfThenElse(b,t,u)   -> SmIfThenElse (stripEffect_expr b,stripEffect_mthd_body t,stripEffect_mthd_body u)
    | SmSelfDestruct e      -> SmSelfDestruct (stripEffect_expr e)
    | SmExpr e              -> SmExpr   (stripEffect_expr e)
    | SmLog(str,args,eopt)  -> SmLog (str, L.map stripEffect_expr args, eopt)

and stripEffect_varDecl v =
    { varDecl_ty            = v.varDecl_ty
    ; varDecl_id            = v.varDecl_id
    ; varDecl_val           = stripEffect_expr v.varDecl_val }

and stripEffect_aa aa =
    { array_name            = stripEffect_expr aa.array_name
    ; array_index           = stripEffect_expr aa.array_index }

and stripEffect_lexpr = function 
    | LEpArray aa          -> LEpArray (stripEffect_aa aa)

and stripEffect_expr(i,(t,_)) = stripEffect_expr_inner i, t

and stripEffect_fncall fc =
    { call_head             = fc.call_head
    ; call_args             = L.map stripEffect_expr fc.call_args }

and stripEffect_msg m =
    { msg_value             = BO.map stripEffect_expr m.msg_value
    ; msg_reentrance        = L.map stripEffect_stmt m.msg_reentrance  }

and stripEffect_send s =
    { send_cntrct           = stripEffect_expr s.send_cntrct
    ; send_mthd             = s.send_mthd
    ; send_args             = L.map stripEffect_expr s.send_args
    ; send_msg              = stripEffect_msg s.send_msg }

and stripEffect_new_expr n =
    { new_head              = n.new_head
    ; new_args              = L.map stripEffect_expr n.new_args
    ; new_msg               = stripEffect_msg n.new_msg }

and stripEffect_expr_inner = function 
    | EpTrue                -> EpTrue
    | EpFalse               -> EpFalse
    | EpDecLit256 d         -> EpDecLit256 d
    | EpDecLit8 d           -> EpDecLit8 d
    | EpNow                 -> EpNow
    | EpFnCall fc           -> EpFnCall (stripEffect_fncall fc)
    | EpIdent str           -> EpIdent str
    | EpParen e             -> EpParen (stripEffect_expr e)
    | EpNew e               -> EpNew (stripEffect_new_expr e)
    | EpSend send           -> EpSend (stripEffect_send send)
    | EpLand(a,b)           -> EpLand (stripEffect_expr a, stripEffect_expr b)
    | EpLt(a,b)             -> EpLt (stripEffect_expr a, stripEffect_expr b)
    | EpGt(a,b)             -> EpGt (stripEffect_expr a, stripEffect_expr b)
    | EpNeq(a,b)            -> EpNeq (stripEffect_expr a, stripEffect_expr b)
    | EpEq(a,b)             -> EpEq (stripEffect_expr a, stripEffect_expr b)
    | EpAddr a              -> EpAddr (stripEffect_expr a)
    | EpNot e               -> EpNot (stripEffect_expr e)
    | EpArray l             -> EpArray (stripEffect_lexpr l)
    | EpValue               -> EpValue
    | EpSender              -> EpSender
    | EpThis                -> EpThis
    | EpSingleDeref e       -> EpSingleDeref (stripEffect_expr e)
    | EpTupleDeref  e       -> EpTupleDeref (stripEffect_expr e)
    | EpPlus(a,b)           -> EpPlus (stripEffect_expr a, stripEffect_expr b)
    | EpMinus(a,b)          -> EpMinus (stripEffect_expr a, stripEffect_expr b)
    | EpMult(a,b)           -> EpMult (stripEffect_expr a, stripEffect_expr b)
    | EpBalance e           -> EpBalance (stripEffect_expr e)

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

let has_distinct_cntrct_names (cns : unit cntrct idx_list) : bool =
    let cn_names    = (L.map(fun(_,b)->b.cntrct_name)cns) in
    L.length cns=L.length(BL.unique cn_names)


let assignTys (tops : unit toplevel idx_list) : ty toplevel idx_list =
    let cntrcts                 = filter_map (function  | Cntrct c -> Some c
                                                        | _        -> None   ) tops in
    assert(has_distinct_cntrct_names(cntrcts));
    let tys                     = map typeof_cntrct cntrcts in
    let evs : evnt idx_list    = filter_map (function  | Event e  -> Some e
                                                        | _        -> None   ) tops in
    map stripEffect (map (assignTy_toplevel tys evs) tops)
