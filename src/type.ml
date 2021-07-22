open Printf 
open Misc 
open Syntax
open IndexList
open TypeEnv
open CodegenEnv

module BL   = BatList
module BS   = BatString
module BO   = BatOption
module L    = List

let reserved x                  =   BS.starts_with x "pre_" 
let reserved_var (TyVar(id,_))  =   reserved id
let reserved_cn  cn             =   reserved cn.cntrct_id
let reserved_exists             =   BL.exists reserved_var 
let reserved_args(cn:'a cntrct) =   BL.exists reserved_var cn.cntrct_args  
let check_reserved x            =   if reserved x           then err "Names 'pre_..' are reserved."
let check_reserved_exists vars  =   if reserved_exists vars then err "Names 'pre_..' are reserved."
let check_reserved_args cn      =   if reserved_args cn     then err "Names 'pre_..' are reserved."
let check_reserved_cn   cn      =   if reserved_cn cn       then err "Names 'pre_..' are reserved."

let assert_tyeqv l r            =   assert (get_ty l=get_ty r) 

let typeof_mthd m               =   match m.mthd_head with 
    | TyMethod(id,args,ret)           ->  TyMethod(id,L.map ty_of_var args, ret)
    | TyDefault                       ->  TyMethod("", [], TyTuple[]) 

(*
let typeof_cntrct(cn:'a cntrct) =   { id                = cn.cntrct_id
                                    ; tyCnArgs          = L.map ty_of_var cn.cntrct_args
                                    ; tyCnMthds         = L.map typeof_mthd cn.mthds            }
                                    *)
let typeof_cntrct  cn           =   TyCntrct(cn.cntrct_id,L.map ty_of_var cn.cntrct_args, L.map typeof_mthd cn.mthds)
let typeof_cntrcts              =   map typeof_cntrct 

let id_lookup_ty ctx id         =   match lookup_id id ctx with
    | Some tyT                      ->  EpIdent id, tyT
    | None                          ->  err ("unknown identifier "^id)

let tycn_has_name  name         =   function TyCntrct(id,_,_) -> id=name     
let itycn_has_name name itycn   =   match get_ty itycn with TyCntrct(id,_,_) -> id=name 
let is_known_cntrct tycns nm    =   BL.exists (itycn_has_name nm) tycns

let rec is_known_ty tycns         =   function 
    | TyUint256 | TyUint8           ->  true
    | TyBytes32 | TyAddr            ->  true
    | TyBool                        ->  true
    | TyTuple []                    ->  true
    | TyTuple l                     ->  BL.for_all (is_known_ty tycns) l
    | TyRef l                       ->  is_known_ty tycns l
    | TyMap(a,b)                    ->  is_known_ty tycns a && is_known_ty tycns b
    | TyInstnce cn                  ->  is_known_cntrct tycns cn

let arg_has_known_ty tycns        =   function 
    | TyVar(id,ty)                  ->  let ret = is_known_ty tycns ty in
                                        if not ret 
                                            then err("Unknown Type Arg "^string_of_ty ty)
                                            else ret

let addTy_mthd_head cns         =   function 
    | TyDefault                     ->  TyDefault
    | TyMethod(id,argTys,retTy)     ->  assert (BL.for_all (arg_has_known_ty (typeof_cntrcts cns)) argTys) ; 
                                        assert (is_known_ty (typeof_cntrcts cns) retTy) ;
                                        TyMethod(id,argTys,retTy)

let call_arg_expectations tycns   =   function 
    | "pre_ecdsarecover"            ->  (=) [TyBytes32;TyUint8;TyBytes32;TyBytes32]
    | "keccak256"                   ->  konst true
    | "iszero"                      ->  fun x -> x=[TyBytes32]||x=[TyUint8]||x=[TyUint256]||x=[TyBool]||x=[TyAddr]
    | name                          ->  let cnIdx       = lookup_idx (tycn_has_name name) tycns in
                                        match lookup_index cnIdx tycns with TyCntrct(_,tyCnArgs,_) -> 
                                        (=) tyCnArgs

let typecheck  (ty,(_,t))       =   assert (ty = t)
let typechecks tys actual       =   L.for_all2 (fun ty (_,a)-> ty=a) tys actual

let check_args_match tycns args   =   function 
    | Some m                        ->  assert (call_arg_expectations tycns m (L.map get_ty args))
    | None                          ->  assert (isNil (L.map get_ty args))

let rec addTy_call cns cname ctx c =
    let argTys  = L.map (addTy_expr cns cname ctx) c.call_args in
    check_args_match (typeof_cntrcts cns) argTys (Some c.call_id) ; 
    let reT     = match c.call_id with
        | "value" when true         ->  TyUint256 (* check the arg is 'msg' *) 
        | "pre_ecdsarecover"        ->  TyAddr
        | "keccak256"               ->  TyBytes32
        | "iszero"                  ->  begin match argTys with
            | [arg]                     ->  TyBool
            | _                         ->  err "should not happen" end 
        | cnname when true          ->  let i,cn = lookup_icn_of_icns cns cnname in 
                                        typeof_cntrct cn 
        | _                         ->  err "addTy_call: should not happen" in
    { call_id   = c.call_id 
    ; call_args = argTys    }, reT


and addTy_expr cns cname ctx (expr,()) =    match expr with
 (* | SmAbort                       ->  SmAbort         , TyVoid *)
    | TmUnit                        ->  TmUnit          , TyTuple[] 
    | EpParen     e                 ->  addTy_expr cns cname ctx e 
    | EpThis                        ->  EpThis          , TyInstnce cname
    | EpTrue                        ->  EpTrue          , TyBool
    | EpFalse                       ->  EpFalse         , TyBool
    | EpSender                      ->  EpSender        , TyAddr
    | EpNow                         ->  EpNow           , TyUint256
    | EpUint256   d                 ->  EpUint256   d   , TyUint256
    | EpUint8     d                 ->  EpUint8     d   , TyUint8
    | EpValue                       ->  EpValue         , TyUint256
    | EpAddr      e                 ->  let e       =   addTy_expr cns cname ctx e          in
                                        EpAddr e        , TyAddr
    | EpCall    c                   ->  let c,ty    =   addTy_call cns cname ctx c          in
                                        EpCall c        , ty
    | EpIdent     s                 ->  check_reserved s ; 
                                        id_lookup_ty ctx s
    | EpBalance   e                 ->  let e       =   addTy_expr cns cname ctx e          in
                                        assert (tyeqv TyAddr (get_ty e));
                                        EpBalance e     , TyUint256
    | EpNew       n                 ->  let n,nm    =   addTy_new cns cname ctx n           in
                                        check_reserved nm;  
                                        EpNew n         , TyInstnce nm
    | EpLAnd (l, r)                 ->  let l       =   addTy_expr cns cname ctx l          in
                                        typecheck (TyBool,l);
                                        let r       =   addTy_expr cns cname ctx r          in
                                        typecheck (TyBool,r); 
                                        EpLAnd(l,r)     , TyBool
    | EpLT (l, r)                   ->  let l       =   addTy_expr cns cname ctx l          in
                                        let r       =   addTy_expr cns cname ctx r          in
                                        assert_tyeqv l r ; 
                                        EpLT(l,r)       , TyBool
    | EpGT (l, r)                   ->  let l       =   addTy_expr cns cname ctx l          in
                                        let r       =   addTy_expr cns cname ctx r          in
                                        assert_tyeqv l r ; 
                                        EpGT (l, r)     , TyBool
    | EpNEq (l, r)                  ->  let l       =   addTy_expr cns cname ctx l          in
                                        let r       =   addTy_expr cns cname ctx r          in
                                        assert_tyeqv l r ;
                                        EpNEq (l, r)    , TyBool
    | EpEq (l, r)                   ->  let l       =   addTy_expr cns cname ctx l          in
                                        let r       =   addTy_expr cns cname ctx r          in
                                        assert_tyeqv l r ; 
                                        EpEq (l, r)     , TyBool
    | EpPlus (l, r)                 ->  let l       =   addTy_expr cns cname ctx l          in
                                        let r       =   addTy_expr cns cname ctx r          in
                                        assert_tyeqv l r ;
                                        EpPlus (l, r)   , get_ty l
    | EpMinus (l, r)                ->  let l       =   addTy_expr cns cname ctx l          in
                                        let r       =   addTy_expr cns cname ctx r          in
                                        assert_tyeqv l r; 
                                        EpMinus (l, r)  , get_ty l
    | EpMult (l, r)                 ->  let l       =   addTy_expr cns cname ctx l          in
                                        let r       =   addTy_expr cns cname ctx r          in
                                        assert_tyeqv l r; 
                                        EpMult (l, r)   , snd l
    | EpNot  e                      ->  let e       =   addTy_expr cns cname ctx e          in
                                        assert (get_ty e=TyBool) ; 
                                        EpNot e         , TyBool
    | EpArray a                     ->  let e       =   addTy_expr cns cname ctx a.arrId    in
                                        begin match get_ty e with | TyMap(kT,vT)  ->  
                                        let idx,ty  =   addTy_expr cns cname ctx a.arrIndex in 
                                        assert (tyeqv kT ty) ; 
                                        EpArray { arrId    = e
                                                ; arrIndex = idx,ty }, vT end  
    | EpSend sd                     ->  let msg     =   addTy_expr  cns cname ctx sd.sd_msg in
                                        let cn      =   addTy_expr cns cname ctx sd.sd_cn   in
                                        begin match sd.sd_mthd with
                                        | Some m ->  
                                        let TyMethod(_,_,tyRet)  =   find_tyMthd (typeof_cntrcts cns) m      in            
                                        let args    =   L.map(addTy_expr cns cname ctx)sd.sd_args in                
                                        let ref     =   EpSend  { sd_cn     = cn                                        
                                                                ; sd_mthd   = sd.sd_mthd                                
                                                                ; sd_args   = args                                      
                                                                ; sd_msg    = msg }, TyRef tyRet  in              
                                        (match tyRet with                                                               
                                            | TyTuple[]    -> ref                                                          
                                            | ty           -> EpDeref ref, ty )                                            
                                        | None ->   assert (sd.sd_args=[]) ; 
                                                    EpSend { sd_cn      = cn
                                                           ; sd_mthd    = None
                                                           ; sd_args    = []
                                                           ; sd_msg     = msg }, TyTuple[]  end

and addTy_new cns cname tenv e =
    let msg'        =   addTy_expr cns cname tenv e.new_msg in
    let args'       =   L.map (addTy_expr cns cname tenv) e.new_args in
    { new_id        =   e.new_id
    ; new_args      =   args'
    ; new_msg       =   msg'          }, e.new_id 

and addTy_lexpr cns cname ctx (LEpArray aa) = 
    let e = addTy_expr cns cname ctx aa.arrId in
    match get_ty e with
    | TyMap (kT,vT)     ->  let idx,ty = addTy_expr cns cname ctx aa.arrIndex in
                            LEpArray { arrId=e; arrIndex=idx,ty }  

and addTy_return cns cname ctx ret =
    let retTyexpr   =   addTy_expr cns cname ctx ret.ret_expr   in
    assert (tyeqv (lookup_retTy ctx) (get_ty retTyexpr)); 
    { ret_expr      =   retTyexpr
    ; ret_cont      =   addTy_expr cns cname ctx ret.ret_cont }

and addTy_decl cns cname ctx vd =
    let v           =   addTy_expr cns cname ctx vd.declVal in
    let id          =   vd.declId     in
    let ty          =   vd.declTy     in
    check_reserved id ; assert (is_known_ty (typeof_cntrcts cns) ty);
    { declTy    = ty
    ; declId    = id
    ; declVal   = v  }, add_var ctx id ty  

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
                            let tys     = L.map(fun ea -> ty_of_var ea.arg)ev.tyEvArgs    in
                            assert(typechecks tys args) ;
                            SmLog(nm,args,Some ev), ctx

and addTy_stmts cns cname ctx = function 
    | []                ->  []
    | stmt::rest        ->  let stmt,ctx = addTy_stmt cns cname ctx stmt in
                            stmt :: addTy_stmts cns cname ctx rest






(* AssignTy Method / Contract / Toplevel  *) 
(* Default Method Returns Unit(==EmptyTuple) is a specification *) 
let retTy_of_mthd = function 
    | TyMethod(_,_,retTy)   -> retTy
    | TyDefault             -> TyTuple[]

let addTy_mthd cns cn_name ctx (m:unit mthd) =
    let retTy       =   retTy_of_mthd  m.mthd_head      in
    let args        =   args_of_mthd  m.mthd_head       in
    let binds       =   binds_of_args args              in
    let ctx'        =   add_retTy ctx retTy             in
    let ctx''       =   add_block ctx' binds            in
    check_reserved_exists args; 
    { mthd_head     =   addTy_mthd_head cns       m.mthd_head
    ; mthd_body     =   addTy_stmts cns cn_name ctx''  m.mthd_body }

let has_distinct_sigs (cn:unit cntrct) =
    let mthds       =   cn.mthds in
    let sigs        =   L.map (fun m -> match m.mthd_head with
                              | TyDefault   -> None
                              | tyM         -> Some (string_of_tyMthd tyM) ) mthds in
    let uniq_sigs   =   BL.unique sigs in
    L.length sigs=L.length uniq_sigs

let addTy_cntrct cns (evs: tyEvnt idx_list) cn =
    check_reserved_cn    cn; 
    check_reserved_args  cn; 
    assert (BL.for_all(arg_has_known_ty (typeof_cntrcts cns))cn.cntrct_args && has_distinct_sigs cn)  ; 
    let ctx  = add_block (add_evnts empty_ctx (values evs)) (binds_of_args cn.cntrct_args) in
    { cntrct_id     =   cn.cntrct_id
    ; cntrct_args   =   cn.cntrct_args
    ; mthds         =   L.map(addTy_mthd cns cn.cntrct_id ctx)cn.mthds }

let addTy_toplevel cns (evs:tyEvnt idx_list) = function 
    | Cntrct c      -> Cntrct (addTy_cntrct cns evs c)
    | Event  e      -> Event e

let has_distinct_cntrct_names (cns : unit cntrct idx_list) : bool =
    let cn_names    = (L.map(fun(_,b)->b.cntrct_id)cns) in
    L.length cns=L.length(BL.unique cn_names)

let addTys (tops : unit toplevel idx_list) : ty toplevel idx_list =
    let cntrcts                 = filter_map (function  | Cntrct c -> Some c
                                                        | _        -> None   ) tops in
    assert(has_distinct_cntrct_names(cntrcts));
    let tycns                   = map typeof_cntrct cntrcts in
    let evs : tyEvnt idx_list   = filter_map (function  | Event e  -> Some e
                                                        | _        -> None   ) tops in
    map (addTy_toplevel cntrcts evs) tops
