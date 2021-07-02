(* TODO : PEN COMPILER *) 
(* 
 * REMOVAL OF Returning Multiple Values 
 * ADDING  OF Product Types / Prouduct Terms *) 




open Printf 

open Misc 
open Syntax
open IndexList
open TypeEnv

module BL   = BatList
module BS   = BatString
module BO   = BatOption
module L    = List
module Eth  = Crypto


let reserved x                  =   BS.starts_with x "pre_" 
let reserved_var (var:tyVar)            =   reserved var.id
let reserved_cn  cn             =   reserved cn.cntrct_id
let reserved_exists             =   BL.exists reserved_var 
let reserved_args (cn:'a cntrct)            =   BL.exists reserved_var cn.cntrct_args  
let check_reserved x            =   if reserved x           then err "Names 'pre_..' are reserved."
let check_reserved_exists vars  =   if reserved_exists vars then err "Names 'pre_..' are reserved."
let check_reserved_args cn      =   if reserved_args cn     then err "Names 'pre_..' are reserved."
let check_reserved_cn   cn      =   if reserved_cn cn       then err "Names 'pre_..' are reserved."
  
let typeof_mthd m               =   match m.mthd_head with
  | Method m                    ->  { tyRet             = m.mthd_retTy
                                    ; id                = m.mthd_name
                                    ; tyArgs            = L.map (fun x->x.ty) m.mthd_args }
  | Default                     ->  { tyRet             = TyTuple[]
                                    ; id                = "" 
                                    ; tyArgs            = []    }

let typeof_cntrct (cn:'a cntrct)            =   { id                = cn.cntrct_id
                                    ; tyCnArgs          = L.map (fun x -> x.ty) cn.cntrct_args
                                    ; tyCnMthds         = L.map typeof_mthd cn.mthds  } 
    

let id_lookup_ty ctx id         =   match lookup_id id ctx with
    | Some tyT                      -> EpIdent id, tyT
    | None                          -> err ("unknown identifier "^id)

let is_known_cntrct cns nm      =   BL.exists (fun(_,(tycn:tyCntrct))->tycn.id=nm) cns

let rec is_known_ty cns         =   function 
    | TyRef l                       ->  is_known_ty cns l
    | TyTuple []                    ->  true
    | TyTuple l                     ->  BL.for_all (is_known_ty cns) l
    | TyMap(a,b)                    ->  is_known_ty cns a && is_known_ty cns b
    | TyCntrct cn                   ->  is_known_cntrct cns cn
    | TyInstnce cn                  ->  is_known_cntrct cns cn
    | TyUint256                     ->  true
    | TyUint8                       ->  true
    | TyBytes32                     ->  true
    | TyAddr                        ->  true
    | TyBool                        ->  true
    | TyUnit                        ->  true
    | TyVoid                        ->  true

let arg_has_known_ty cns arg    =   let ret = is_known_ty cns arg.ty in
                                    if not ret 
                                        then err("Unknown Type Arg "^string_of_ty arg.ty)
                                        else ret

let retTy_is_known cns m        =   is_known_ty cns m.mthd_retTy

let addTy_mthd_head cns         =   function 
    | Default                       ->  Default
    | Method m                      ->  assert (BL.for_all(arg_has_known_ty cns)m.mthd_args) ; 
                                        assert (retTy_is_known cns m) ;
                                        Method m

let call_arg_expectations cns   =   function 
    | "pre_ecdsarecover"            ->  (=) [TyBytes32;TyUint8;TyBytes32;TyBytes32]
    | "keccak256"                   ->  konst true
    | "iszero"                      ->  fun x -> x=[TyBytes32]||x=[TyUint8]||x=[TyUint256]||x=[TyBool]||x=[TyAddr]
    | cnname                        ->  let cnIdx   = lookup_idx (fun (tycn:tyCntrct)->tycn.id=cnname) cns in
                                        let tyCn    = lookup_index cnIdx cns in
                                        (=) tyCn.tyCnArgs

let typecheck  (ty,(_,t))       =   assert (ty = t)
let typechecks tys actual       =   L.for_all2 (fun ty (_,a)-> ty=a) tys actual

let assert_tyeqv l r            =   assert (get_ty l=get_ty r) 

let check_args_match cns args   =   function 
    | Some m                        ->  assert (call_arg_expectations cns m (L.map get_ty args))
    | None                          ->  assert (isNil (L.map get_ty args))


let rec addTy_call cns cname ctx c =
    let args   = L.map (addTy_expr cns cname ctx) c.call_args in
    check_args_match cns args (Some c.call_head) ; 
    let reT    = match c.call_head with
        | "value" when true         -> TyUint256 (* check the arg is 'msg' *) 
        | "pre_ecdsarecover"        -> TyAddr
        | "keccak256"               -> TyBytes32
        | "iszero"                  -> begin match args with
            | [arg]                     -> TyBool
            | _                         -> err "should not happen" end 
        | cnname when true          -> TyCntrct cnname (* check contract exists *) 
        | _                         -> err "addTy_call: should not happen" in
    {call_head=c.call_head;call_args=args},reT

and addTy_msg cns cname ctx msg =
    let expr    = BO.map (addTy_expr cns cname ctx) msg.msg_value in
    let stmts   = addTy_stmts cns cname ctx msg.msg_reentrance in
    { msg_value      = expr
    ; msg_reentrance = stmts }

and addTy_expr cns cname ctx (expr,()) = 
    match expr with
    | EpParen e         ->  addTy_expr cns cname ctx e 
    | EpDeref _         ->  err "Deref not supposed" 
    | EpThis            ->  EpThis          , TyInstnce cname
    | EpTrue            ->  EpTrue          , TyBool
    | EpFalse           ->  EpFalse         , TyBool
    | EpSender          ->  EpSender        , TyAddr
    | EpNow             ->  EpNow           , TyUint256
    | EpDecLit256 d     ->  EpDecLit256 d   , TyUint256
    | EpDecLit8   d     ->  EpDecLit8   d   , TyUint8
    | EpValue           ->  EpValue         , TyUint256
    | EpAddr e          ->  let e   = addTy_expr cns cname ctx e          in
                            EpAddr e        , TyAddr
    | EpBalance e       ->  let e   = addTy_expr cns cname ctx e          in
                            assert (acceptable_as TyAddr (get_ty e));
                            EpBalance e     , TyUint256
    | EpFnCall    c     ->  let c,ty= addTy_call cns cname ctx c in
                            EpFnCall c      , ty
    | EpIdent     s     ->  check_reserved s ; 
                            id_lookup_ty ctx s
    | EpNew n           ->  let n,nm= addTy_new_expr cns cname ctx n in
                            check_reserved nm;  
                            EpNew n         , TyInstnce nm
    | EpLAnd (l, r)     ->  let l   = addTy_expr cns cname ctx l          in
                            typecheck (TyBool,l);
                            let r   = addTy_expr cns cname ctx r          in
                            typecheck (TyBool,r); 
                            EpLAnd(l,r)     , TyBool
    | EpLT (l, r)       ->  let l   = addTy_expr cns cname ctx l          in
                            let r   = addTy_expr cns cname ctx r          in
                            assert_tyeqv l r ; 
                            EpLT(l,r)       , TyBool
    | EpGT (l, r)       ->  let l   = addTy_expr cns cname ctx l          in
                            let r   = addTy_expr cns cname ctx r          in
                            assert_tyeqv l r ; 
                            EpGT (l, r)     , TyBool
    | EpNeq (l, r)      ->  let l   = addTy_expr cns cname ctx l          in
                            let r   = addTy_expr cns cname ctx r          in
                            assert_tyeqv l r ;
                            EpNeq (l, r)    , TyBool
    | EpEq (l, r)       ->  let l   = addTy_expr cns cname ctx l          in
                            let r   = addTy_expr cns cname ctx r          in
                            assert_tyeqv l r ; 
                            EpEq (l, r)     , TyBool
    | EpPlus (l, r)     ->  let l   = addTy_expr cns cname ctx l          in
                            let r   = addTy_expr cns cname ctx r          in
                            assert_tyeqv l r ;
                            EpPlus (l, r)   , get_ty l
    | EpMinus (l, r)    ->  let l   = addTy_expr cns cname ctx l          in
                            let r   = addTy_expr cns cname ctx r          in
                            assert_tyeqv l r; 
                            EpMinus (l, r)  , get_ty l
    | EpMult (l, r)     ->  let l   = addTy_expr cns cname ctx l          in
                            let r   = addTy_expr cns cname ctx r          in
                            assert_tyeqv l r; 
                            EpMult (l, r)   , snd l
    | EpNot  e          ->  let e   = addTy_expr cns cname ctx e          in
                            assert (get_ty e=TyBool) ; 
                            EpNot e         , TyBool
    | EpArray aa        ->  let e   = addTy_expr cns cname ctx (read_array aa).arrIdent in
                            begin match get_ty e with
                            | TyMap(kT,vT)-> let aaidx              = (read_array aa).arrIndex  in 
                                             let idx,idxTy = addTy_expr cns cname ctx aaidx   in 
                                             assert (acceptable_as kT idxTy) ; 
                                             EpArray(LEpArray{ arrIdent = e
                                                             ; arrIndex = idx,idxTy }), vT
                            | _           -> err "index access has to be on mappings"   end
    | EpSend send       ->  let msg = addTy_msg  cns cname ctx send.send_msg          in
                            let cn' = addTy_expr cns cname ctx send.send_cntrct       in
                            begin match send.send_mthd with
                            | Some m -> let tyMthd  =   find_tyMthd cns m                                   in 
                                        let tyRet   =   tyMthd.tyRet                                        in
                                        let args    =   L.map (addTy_expr cns cname ctx) send.send_args   in
                                        let ref     =   EpSend  { send_cntrct   = cn'
                                                                ; send_mthd     = send.send_mthd
                                                                ; send_args     = args
                                                                ; send_msg      = msg       }, TyRef tyRet  in
                                        (match tyRet with
                                         | TyTuple[]    -> ref 
                                         | ty           -> EpDeref ref, ty )
                            | None ->   assert (send.send_args = []) ; 
                                        EpSend { send_cntrct    = cn'
                                               ; send_mthd      = None
                                               ; send_args      = []
                                               ; send_msg  = msg }, TyTuple[]  end

and addTy_new_expr cns cname tenv e =
    let msg'        =   addTy_msg cns cname tenv e.new_msg in
    let args'       =   L.map (addTy_expr cns cname tenv) e.new_args in
    { new_head      =   e.new_head
    ; new_args      =   args'
    ; new_msg       =   msg'          }, e.new_head 

and addTy_lexpr cns cname ctx = function 
    | LEpArray aa       ->
    let e = addTy_expr cns cname ctx aa.arrIdent in
    begin match get_ty e with
    | TyMap (kT,vT)     ->  let idx,ty = addTy_expr cns cname ctx aa.arrIndex in
                            LEpArray { arrIdent=e; arrIndex=idx,ty }
    | _                 ->  err ("unknown array") end

and addTy_return cns cname ctx ret =
    let retTyexpr   =   BO.map (addTy_expr cns cname ctx) ret.ret_expr in
    let retTyCheck  =   lookup_retTyCheck ctx in
    assert (retTyCheck(BO.map get_ty retTyexpr)); 
    { ret_expr      =   retTyexpr
    ; ret_cont      =   addTy_expr cns cname ctx ret.ret_cont }

and addTy_decl cns cname ctx vd =
    let v           =   addTy_expr cns cname ctx vd.declVal in
    let id          =   vd.declId     in
    let ty          =   vd.declTy     in
    check_reserved id ;
    assert (is_known_ty cns ty);
    let ctx'        =   add_var ctx id ty in
    let vd'         =   { declTy    = ty
                        ; declId    = id
                        ; declVal   = v  } in
    vd', ctx'

and addTy_stmt cns cname ctx = function 
    | SmAbort           ->  SmAbort         , ctx
    | SmReturn r        ->  let r       = addTy_return      cns cname ctx  r        in
                            SmReturn r      , ctx
    | SmAssign(l,r)     ->  let l       = addTy_lexpr       cns cname ctx  l        in
                            let r       = addTy_expr        cns cname ctx  r        in
                            SmAssign(l,r)   , ctx
    | SmIfThen(b,t)     ->  let b       = addTy_expr        cns cname ctx  b        in
                            let t       = addTy_stmts       cns cname ctx  t        in
                            SmIfThen(b,t)   , ctx
    | SmIf(b,t,f)       ->  let b       = addTy_expr        cns cname ctx  b        in
                            let t       = addTy_stmts       cns cname ctx  t        in
                            let f       = addTy_stmts       cns cname ctx  f        in
                            SmIf(b,t,f)     , ctx
    | SmSlfDstrct e     ->  let e       = addTy_expr        cns cname ctx  e        in
                            SmSlfDstrct e   , ctx
    | SmDecl v          ->  let v,ctx   = addTy_decl        cns cname ctx  v        in
                            SmDecl v        , ctx
    | SmExpr e          ->  let e       = addTy_expr        cns cname ctx  e        in
                            assert(get_ty e=TyTuple[]) ;
                            SmExpr e        , ctx
    | SmLog(nm,args,_)  ->  let args    = L.map (addTy_expr cns cname ctx) args     in
                            let ev      = lookup_evnt nm ctx                        in
                            let tys     = L.map(fun ea -> ea.arg.ty)ev.tyEvArgs    in
                            assert(typechecks tys args) ;
                            SmLog(nm,args,Some ev), ctx

and addTy_stmts cns cname ctx = function 
    | []                ->  []
    | stmt::rest        ->  let stmt,ctx = addTy_stmt cns cname ctx stmt in
                            stmt :: addTy_stmts cns cname ctx rest


(* Termination *) 

type termination            =
                            | OnTheWay 
                            | ReturnBySize of int 
                            | JustStop

let rec is_terminating      = function 
    | SmAbort                   -> [JustStop]
    | SmSlfDstrct _             -> [JustStop]
    | SmAssign    _             -> [OnTheWay]
    | SmDecl      _             -> [OnTheWay]
    | SmExpr      _             -> [OnTheWay]
    | SmLog       _             -> [OnTheWay]
    | SmIfThen(_,b)             -> are_terminating b  @ [OnTheWay] (* there is a continuation if the condition does not hold. *)
    | SmIf(_,bT,bF)             -> are_terminating bT @ (are_terminating bF)
    | SmReturn ret              -> begin match ret.ret_expr with
        | Some _                    -> [ReturnBySize 1]
        | None                      -> [ReturnBySize 0] end

and are_terminating stmts =
  let last_stmt = BL.last stmts in
  is_terminating last_stmt



(* AssignTy Method / Contract / Toplevel  *) 

(* Default Method Returns Unit(==EmptyTuple) is a specification *) 

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


let addTy_mthd cns cn_name ctx (m:unit mthd) =
    assert (L.for_all (function 
                         | OnTheWay         -> false
                         | ReturnBySize 0   -> mthd_is_returning_void m
                         | ReturnBySize 1   -> not (mthd_is_returning_void m)
                         | ReturnBySize _   -> err "multiple vals return not supported yry"
                         | JustStop         -> true )  (are_terminating m.mthd_body)) ; 
    let margs       = args_of_mthd m.mthd_head in
    check_reserved_exists margs; 
    let retTyCheck  = retTyCheck_of_mthd m.mthd_head in
    let ctx'        = add_retTyChkr (add_block ctx margs) retTyCheck in 
    { mthd_head         = addTy_mthd_head cns m.mthd_head
    ; mthd_body         = addTy_stmts cns cn_name ctx' m.mthd_body }

let has_distinct_sigs (cn:unit cntrct) =
    let mthds       =   cn.mthds in
    let sigs        =   L.map (fun m -> match m.mthd_head with
                              | Method m' -> Some (Eth.string_of_tyMthd m')
                              | Default   -> None) mthds in
    let uniq_sigs   =   BL.unique sigs in
    L.length sigs=L.length uniq_sigs


let addTy_cntrct cns (evs: tyEvnt idx_list) cn =
    check_reserved_cn    cn; 
    check_reserved_args  cn; 
    assert (BL.for_all(arg_has_known_ty cns)cn.cntrct_args && has_distinct_sigs cn)  ; 
    let ctx  = add_block (add_evnts empty_ctx evs) cn.cntrct_args in
    { cntrct_id     = cn.cntrct_id
    ; cntrct_args   = cn.cntrct_args
    ; mthds         = L.map(addTy_mthd cns cn.cntrct_id ctx)cn.mthds }

let addTy_toplevel cns (evs:tyEvnt idx_list) = function 
    | Cntrct c      -> Cntrct (addTy_cntrct cns evs c)
    | Event  e      -> Event e



(* Assign Type *) 

let has_distinct_cntrct_names (cns : unit cntrct idx_list) : bool =
    let cn_names    = (L.map(fun(_,b)->b.cntrct_id)cns) in
    L.length cns=L.length(BL.unique cn_names)


let addTys (tops : unit toplevel idx_list) : ty toplevel idx_list =
    let cntrcts                 = filter_map (function  | Cntrct c -> Some c
                                                        | _        -> None   ) tops in
    assert(has_distinct_cntrct_names(cntrcts));
    let tys                     = map typeof_cntrct cntrcts in
    let evs : tyEvnt idx_list    = filter_map (function   | Event e  -> Some e
                                                        | _        -> None   ) tops in
    map (addTy_toplevel tys evs) tops
