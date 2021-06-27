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

module BL   = BatList
module BS   = BatString
module BO   = BatOption
module L    = List
module Eth  = Ethereum



let id_lookup_ty tenv id        =   match lookup tenv id with
    | Some tyT                      -> EpIdent id, tyT
    | None                          -> err ("unknown identifier "^id)

let is_known_cntrct tyCns nm    =   BL.exists (fun(_,i)->i.tyCntrct_name=nm) tyCns

let rec is_known_ty tyCns       =   function 
    | TyRef l                       ->  is_known_ty tyCns l
    | TyTuple []                    ->  true
    | TyTuple l                     ->  BL.for_all (is_known_ty tyCns) l
    | TyMap(a,b)                    ->  is_known_ty tyCns a && is_known_ty tyCns b
    | TyCntrct cn                   ->  is_known_cntrct tyCns cn
    | TyInstnce cn                  ->  is_known_cntrct tyCns cn
    | TyUint256                     ->  true
    | TyUint8                       ->  true
    | TyBytes32                     ->  true
    | TyAddr                        ->  true
    | TyBool                        ->  true
    | TyUnit                        ->  true
    | TyVoid                        ->  true

let arg_has_known_ty tyCns arg  =   let ret = is_known_ty tyCns arg.ty in
                                        if not ret then eprintf"arg has Unknown Type %s\n"(string_of_ty arg.ty);
                                        ret

let retTy_is_known tyCns m      =   is_known_ty tyCns m.mthd_retTy

let addTy_mthd_head tyCns       =   function 
    | Default                       ->  Default
    | Method m                      ->  assert (BL.for_all(arg_has_known_ty tyCns)m.mthd_args) ; 
                                        assert (retTy_is_known tyCns m) ;
                                        Method m

let call_arg_expectations tyCns =   function 
    | "pre_ecdsarecover"            ->  (=) [TyBytes32;TyUint8;TyBytes32;TyBytes32]
    | "keccak256"                   ->  fun _ -> true
    | "iszero"                      ->  fun x -> x=[TyBytes32]||x=[TyUint8]||x=[TyUint256]||x=[TyBool]||x=[TyAddr]
    | name                          ->  let idx     = lookup_idx (fun c->c.tyCntrct_name=name) tyCns in
                                        let tyCn    = lookup_index idx tyCns in
                                        (=) tyCn.tyCntrct_args



let typecheck  (expr,(_,t))     = assert (expr = t)
let typechecks exprs actual     = L.for_all2 (fun e (_,a)-> e=a) exprs actual

let get_ty  (_,ty)              = ty
let get_tm  (x,_)               = x
let assert_tyeqv l r            = assert (get_ty l=get_ty r) 


let check_args_match tyCns (args:(ty expr_tm * ty) list)  = function 
    | Some m        ->  assert (call_arg_expectations tyCns m (L.map get_ty args))
    | None          ->  assert (isNil (L.map get_ty args))



let rec addTy_call tyCns cname tyenv (fncall:unit fncall) : ty fncall * ty  =
    let args   = L.map (addTy_expr tyCns cname tyenv) fncall.call_args in
    check_args_match tyCns args (Some fncall.call_head) ; 
    let reT    = match fncall.call_head with
        | "value" when true         -> TyUint256 (* check the arg is 'msg' *) 
        | "pre_ecdsarecover"        -> TyAddr
        | "keccak256"               -> TyBytes32
        | "iszero"                  -> begin match args with
            | [arg]                     -> TyBool
            | _                         -> err "should not happen" end 
        | cn_name when true         -> TyCntrct cn_name (* check contract exists *) 
        | _                         -> err "addTy_call: should not happen" in
    {call_head=fncall.call_head;call_args=args},reT


and addTy_msg tyCns cname tyenv (msg:unit msg) : ty msg =
    let expr  = BO.map (addTy_expr tyCns cname tyenv) msg.msg_value in
    let stmts = addTy_stmts tyCns cname tyenv msg.msg_reentrance in
    { msg_value      = expr
    ; msg_reentrance = stmts }


and addTy_expr tyCns cname tyenv (expr_tm,()) : ty expr =
    match expr_tm with
    | EpThis          ->    EpThis          , TyInstnce cname
    | EpTrue          ->    EpTrue          , TyBool
    | EpFalse         ->    EpFalse         , TyBool
    | EpSender        ->    EpSender        , TyAddr
    | EpNow           ->    EpNow           , TyUint256
    | EpDecLit256 d   ->    EpDecLit256 d   , TyUint256
    | EpDecLit8   d   ->    EpDecLit8   d   , TyUint8
    | EpFnCall    c   ->    let c,ty = addTy_call tyCns cname tyenv c in
                            EpFnCall c      , ty
    | EpIdent     s   ->    if BS.starts_with s "pre_" 
                                then err "names that start with pre_ are reserved" 
                                else id_lookup_ty tyenv s

    | EpParen e       ->    addTy_expr tyCns cname tyenv e 

    | EpNew n         ->    let n',cname'   = addTy_new_expr tyCns cname tyenv n in
                            if BS.starts_with cname' "pre_" then err "names that start with pre_ are reserved"; 
                            EpNew n'        , TyInstnce cname'
    | EpLand (l, r)   ->    let l           = addTy_expr tyCns cname tyenv l in
                            typecheck (TyBool,l);
                            let r           = addTy_expr tyCns cname tyenv r in
                            typecheck (TyBool,r); 
                            EpLand(l,r)    , TyBool
    | EpLt (l, r)     ->
                            let l       = addTy_expr tyCns cname tyenv l in
                            let r       = addTy_expr tyCns cname tyenv r in
                            assert_tyeqv l r ; 
                            EpLt(l,r)      , TyBool
    | EpGt (l, r)     ->
                            let l       = addTy_expr tyCns cname tyenv l in
                            let r       = addTy_expr tyCns cname tyenv r in
                            assert_tyeqv l r ; 
                            EpGt (l, r)    , TyBool
    | EpNeq (l, r)    ->
                            let l       = addTy_expr tyCns cname tyenv l in
                            let r       = addTy_expr tyCns cname tyenv r in
                            assert_tyeqv l r ;
                            EpNeq (l, r), TyBool
    | EpEq (l, r)     ->
                            let l       = addTy_expr tyCns cname tyenv l in
                            let r       = addTy_expr tyCns cname tyenv r in
                            assert_tyeqv l r ; 
                            EpEq (l, r), TyBool
    | EpPlus (l, r)   ->
                            let l       = addTy_expr tyCns cname tyenv l in
                            let r       = addTy_expr tyCns cname tyenv r in
                            assert_tyeqv l r ;
                            EpPlus (l, r), get_ty l
    | EpMinus (l, r)  ->
                            let l       = addTy_expr tyCns cname tyenv l in
                            let r       = addTy_expr tyCns cname tyenv r in
                            assert_tyeqv l r; 
                            EpMinus (l, r), get_ty l
    | EpMult (l, r)   ->
                            let l       = addTy_expr tyCns cname tyenv l in
                            let r       = addTy_expr tyCns cname tyenv r in
                            assert_tyeqv l r; 
                            EpMult (l, r), snd l
    | EpNot  e        ->
                            let e       = addTy_expr tyCns cname tyenv e in
                            assert (get_ty e = TyBool) ; 
                            EpNot e       , TyBool
    | EpAddr e        ->
                            let e       = addTy_expr tyCns cname tyenv e in
                            EpAddr e      , TyAddr
    | EpBalance e     ->
                            let e       = addTy_expr tyCns cname tyenv e in
                            assert (acceptable_as TyAddr (get_ty e));
                            EpBalance e   , TyUint256
    | EpArray aa->
                            let e       = addTy_expr tyCns cname tyenv (read_array aa).arrIdent in
                            begin match get_ty e with
                            | TyMap(kT,vT)-> let aaidx              = (read_array aa).arrIndex in 
                                             let idx,idxTy = addTy_expr tyCns cname tyenv aaidx in 
                                             assert (acceptable_as kT idxTy) ; 
                                             EpArray(LEpArray{ arrIdent = e
                                                             ; arrIndex = idx,idxTy }), vT
                            | _           -> err "index access has to be on mappings"   end
    | EpSend send     ->
                            let msg         = addTy_msg  tyCns cname tyenv send.send_msg    in
                            let cn'         = addTy_expr tyCns cname tyenv send.send_cntrct in
                            begin match send.send_mthd with
                            | Some m -> let tyMthd  =   find_tyMthd tyCns m                 in 
                                        let tyRet   =   tyMthd.tyRet                        in
                                        let args    =   L.map (addTy_expr tyCns cname tyenv) send.send_args in
                                        let ref     =   EpSend  { send_cntrct   = cn'
                                                                ; send_mthd     = send.send_mthd
                                                                ; send_args     = args
                                                                ; send_msg      = msg           }, TyRef tyRet        in
                                        (match tyRet with
                                         | TyTuple[]    -> ref 
                                         | ty           -> EpSingleDeref ref, ty )
                            | None ->
                                        assert (send.send_args = []) ; 
                                        EpSend { send_cntrct    = cn'
                                               ; send_mthd      = None
                                               ; send_args      = []
                                               ; send_msg  = msg }, TyTuple[]  end
    | EpValue         ->    EpValue, TyUint256
    | EpSingleDeref _
    | EpTupleDeref  _ ->    err "DerefEp not supposed (Bamboo Bad Designing)"


and addTy_new_expr tyCns cname tenv e : ty new_expr*string (* name of the cntrct just created *) =
    let msg'        =   addTy_msg tyCns cname tenv e.new_msg in
    let args'       =   L.map (addTy_expr tyCns cname tenv) e.new_args in
    let e'          =   { new_head      = e.new_head
                        ; new_args      = args'
                        ; new_msg       = msg' } in
    (e', e.new_head )

  
and addTy_lexpr tyCns cname tyenv (src:unit lexpr) : ty lexpr =
    match src with
    | LEpArray aa ->let e = addTy_expr tyCns cname tyenv aa.arrIdent in
                            begin match get_ty e with
                            | TyMap (kT,vT) ->
                               let idx,idx_ty = addTy_expr tyCns cname tyenv aa.arrIndex in
                               LEpArray { arrIdent    = e 
                                        ; arrIndex   = idx,idx_ty  }
                            | _             -> err ("unknown array") end


and addTy_return tyCns cname tyenv (ret:unit return) : ty return =
    let ret_tyexpr  = BO.map (addTy_expr tyCns cname tyenv) ret.ret_expr in
    let retTyCheck  = lookup_retTyCheck tyenv in
    assert (retTyCheck(BO.map get_ty ret_tyexpr)); 
    { ret_expr      = ret_tyexpr
    ; ret_cont      = addTy_expr tyCns cname tyenv ret.ret_cont }


and addTy_varDecl tyCns cname tyenv (vd:unit varDecl): ty varDecl * tyEnv =
    let v       = addTy_expr tyCns cname tyenv vd.varDecl_val in
    let id      = vd.varDecl_id     in
    let ty      = vd.varDecl_ty     in
    if BS.starts_with id "pre_" then err "Names \"pre_..\" are reserved" ;
    assert (is_known_ty tyCns ty);
    let tyenv'  = add_var tyenv id ty in
    let vd'     =   { varDecl_ty    = ty
                    ; varDecl_id    = id
                    ; varDecl_val   = v  } in
    vd', tyenv'


and addTy_stmt tyCns cname tyenv (src:unit stmt) : (ty stmt * tyEnv) =
    match src with
    | SmAbort           ->  SmAbort         , tyenv
    | SmReturn r        ->  let r       = addTy_return   tyCns cname tyenv r in
                            SmReturn r      , tyenv
    | SmAssign(l,r)     ->  let l       = addTy_lexpr    tyCns cname tyenv l in
                            let r       = addTy_expr     tyCns cname tyenv r in
                            SmAssign(l,r)   , tyenv
    | SmIfThen(b,t)     ->  let b       = addTy_expr     tyCns cname tyenv b in
                            let t       = addTy_stmts    tyCns cname tyenv t in
                            SmIfThen(b,t)   , tyenv
    | SmIf(b,t,f)       ->  let b       = addTy_expr     tyCns cname tyenv b in
                            let t       = addTy_stmts    tyCns cname tyenv t in
                            let f       = addTy_stmts    tyCns cname tyenv f in
                            SmIf(b,t,f)     , tyenv
    | SmSelfDestruct e  ->  let e       = addTy_expr     tyCns cname tyenv e in
                            SmSelfDestruct e, tyenv
    | SmVarDecl v       ->  let v,tyenv = addTy_varDecl  tyCns cname tyenv v in
                            SmVarDecl v     , tyenv
    | SmExpr e          ->  let e       = addTy_expr     tyCns cname tyenv e in
                            assert(get_ty e = TyTuple([])) ;
                            SmExpr e        , tyenv
    | SmLog(nm,args,_)  ->  let args    = L.map(addTy_expr tyCns cname tyenv) args in
                            let ev      = lookup_evnt tyenv nm in
                            let tys     = L.map(fun ea -> ea.arg.ty) ev.evnt_args in
                            assert (typechecks tys args) ;
                            SmLog(nm,args,Some ev), tyenv


and addTy_stmts tyCns cname tyenv = function (* unit stmt list -> ty stmt list*) 
    | []            ->  []
    | stmt::rest    ->  let stmt,tyenv = addTy_stmt tyCns cname tyenv stmt in
                        stmt :: addTy_stmts tyCns cname tyenv rest


(* Termination *) 

type termination            =
                            | OnTheWay 
                            | ReturnBySize of int 
                            | JustStop

let rec is_terminating      = function 
    | SmAbort                   -> [JustStop]
    | SmSelfDestruct _          -> [JustStop]
    | SmReturn ret              -> begin match ret.ret_expr with
        | Some _                    -> [ReturnBySize 1]
        | None                      -> [ReturnBySize 0] end
    | SmIfThen(_,b)             -> are_terminating b  @ [OnTheWay] (* there is a continuation if the condition does not hold. *)
    | SmIf(_,bT,bF)             -> are_terminating bT @ (are_terminating bF)
    | SmAssign _                -> [OnTheWay]
    | SmVarDecl _               -> [OnTheWay]
    | SmExpr _                  -> [OnTheWay]
    | SmLog _                   -> [OnTheWay]

and are_terminating stmts =
  let last_stmt = BL.last stmts in
  is_terminating last_stmt




  
(* AssignTy Method / Contract / Toplevel  *) 

(* Default Method Returns Void is a specification *) 

let mthd_is_returning_void (mthd:unit mthd) = match mthd.mthd_head with
    | Default               ->  true
    | Method m              ->  m.mthd_retTy = TyTuple[]

let retTyCheck_of_mthd m ty_inferred = match m, ty_inferred with
    | Default ,Some _       ->  false
    | Default ,None         ->  true
    | Method u,   _         ->  begin match u.mthd_retTy, ty_inferred with
        | tyVoid, None          -> true
        | x, Some y             -> acceptable_as x y
        | _, _                  -> false  end


let addTy_mthd tyCns cn_name tenv (m : unit mthd) =
    assert (L.for_all (function 
                         | OnTheWay         -> false
                         | ReturnBySize 0   -> mthd_is_returning_void m
                         | ReturnBySize 1   -> not (mthd_is_returning_void m)
                         | ReturnBySize _   -> err "multiple vals return not supported yry"
                         | JustStop         -> true )  (are_terminating m.mthd_body)) ; 
    let margs       = args_of_mthd m.mthd_head in
    if BL.exists (fun arg -> BS.starts_with arg.id "pre_") margs 
                then err "names that start with pre_ are reserved"; 
    let retTyCheck  = retTyCheck_of_mthd m.mthd_head in
    let tyenv'      = set_retTyCheck (add_block margs tenv) retTyCheck in 
    { mthd_head = addTy_mthd_head tyCns m.mthd_head
    ; mthd_body = addTy_stmts tyCns cn_name tyenv' m.mthd_body }

let has_distinct_sigs (cn:unit cntrct) =
    let mthds       =   cn.mthds in
    let sigs        =   L.map (fun m -> match m.mthd_head with
                              | Method m' -> Some (Eth.string_of_tyMthd m')
                              | Default   -> None) mthds in
    let uniq_sigs   =   BL.unique sigs in
    L.length sigs=L.length uniq_sigs


let addTy_cntrct tyCns (evs: evnt idx_list)(cn : unit cntrct) 
            : ty cntrct =
    assert (BL.for_all (arg_has_known_ty tyCns) cn.cntrct_args) ; 
    assert (has_distinct_sigs cn);
    let tyenv  = add_block cn.cntrct_args (add_evnts evs empty_tyEnv) in
    if BS.starts_with cn.cntrct_name "pre_" 
        then err "names \"pre_..\" are reserved" ; 
    if BL.exists (fun arg -> BS.starts_with arg.id "pre_") cn.cntrct_args 
        then err "names \"pre_..\" are reserved" ; 
    { cntrct_name     = cn.cntrct_name
    ; cntrct_args     = cn.cntrct_args
    ; mthds           = L.map(addTy_mthd tyCns cn.cntrct_name tyenv)cn.mthds }


let addTy_toplevel tyCns (evs:evnt idx_list) (top:unit toplevel) : ty toplevel 
    = match top with
    | Cntrct c    -> Cntrct (addTy_cntrct tyCns evs c)
    | Event  e    -> Event e











(* Assign Type *) 

let has_distinct_cntrct_names (cns : unit cntrct idx_list) : bool =
    let cn_names    = (L.map(fun(_,b)->b.cntrct_name)cns) in
    L.length cns=L.length(BL.unique cn_names)


let addTys (tops : unit toplevel idx_list) : ty toplevel idx_list =
    let cntrcts                 = filter_map (function  | Cntrct c -> Some c
                                                        | _        -> None   ) tops in
    assert(has_distinct_cntrct_names(cntrcts));
    let tys                     = map typeof_cntrct cntrcts in
    let evs : evnt idx_list    = filter_map (function   | Event e  -> Some e
                                                        | _        -> None   ) tops in
    map (addTy_toplevel tys evs) tops
