open Printf 
open Misc 
open Syntax
open Context
open CodegenEnv

module BL   = BatList
module BS   = BatString
module BO   = BatOption
module L    = List


(***********************************)
(*     Type of Method Name         *)
(***********************************)

let find_tyMthd_in_cn mname = find_by_filter (function TyMthd(i,r,m)when i=mname -> TyMthd(i,r,m)               | _ -> raise Not_found) 
let find_tyMthd       mname = find_by_filter (function TyCn(_,_,tyCnMthds) -> find_tyMthd_in_cn mname tyCnMthds | _ -> raise Not_found)


(***********************************)
(***      Type Equivalence       ***)
(***********************************)

let tyeqv t0 t1                 =   ( t0 = t1 )  ||  ( match t0, t1 with
                                | TyAddr, TyInstnce _   -> true
                                | _     , _             -> false ) 


let assert_tyeqv l r            =   assert (get_ty l=get_ty r) 

let typeof_mthd                 =   function 
    | TmMthd(TyMthd(id,args,ret),_) ->  TyMthd(id, tys_of_vars args, ret)
    | TmMthd(TyDefault,_)           ->  TyMthd("", [], TyTuple[]) 

let typeof_cn  cn               =   TyCn(cn.id,tys_of_vars cn.fields, L.map typeof_mthd cn.mthds)
let typeof_cns                  =   map typeof_cn 

let id_lookup_ty ctx id         =   try TmId id, lookup_id id ctx 
                                    with Not_found -> err("unknown id "^id)  

let tycn_has_name  name         =   function TyCn(id,_,_) -> id=name     
let itycn_has_name name itycn   =   match get_ty itycn with TyCn(id,_,_) -> id=name 
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
    | TyMthd(id,argTys,retTy)     ->  assert (BL.for_all (arg_has_known_ty (typeof_cns cns)) argTys) ; 
                                        assert (is_known_ty (typeof_cns cns) retTy) ;
                                        TyMthd(id,argTys,retTy)

let call_arg_expectations tycns   =   function 
    | "pre_ecdsarecover"            ->  (=) [TyBytes32;TyUint8;TyBytes32;TyBytes32]
    | "keccak256"                   ->  konst true
    | "iszero"                      ->  fun x -> x=[TyBytes32]||x=[TyUint8]||x=[TyUint256]||x=[TyBool]||x=[TyAddr]
    | name                          ->  let cn_idx       = lookup_idx (tycn_has_name name) tycns in
                                        match lookup cn_idx tycns with TyCn(_,tyCnArgs,_) -> 
                                        (=) tyCnArgs

let typecheck  (ty,(_,t))       =   assert (ty = t)
let typechecks tys actual       =   L.for_all2 (fun ty (_,a)-> ty=a) tys actual

let check_args_match tycns args   =   function 
    | Some m                        ->  assert (call_arg_expectations tycns m (L.map get_ty args))
    | None                          ->  assert (isNil (L.map get_ty args))

let rec addTy_call cns cname ctx c =
    let argTys  = L.map (addTy_expr cns cname ctx) c.call_args in
    check_args_match (typeof_cns cns) argTys (Some c.call_id) ; 
    let reT     = match c.call_id with
        | "value" when true         ->  TyUint256 (* check the arg is 'msg' *) 
        | "pre_ecdsarecover"        ->  TyAddr
        | "keccak256"               ->  TyBytes32
        | "iszero"                  ->  begin match argTys with
            | [arg]                     ->  TyBool
            | _                         ->  err "should not happen" end 
        | cnname when true          ->  let i,cn = lookup_icn_of_icns cns cnname in 
                                        typeof_cn cn 
        | _                         ->  err "addTy_call: should not happen" in
    { call_id   = c.call_id 
    ; call_args = argTys    }, reT


and addTy_expr cns cname ctx (expr,()) =    match expr with
 (*   | TmAbs(fi,x,tyT1,t2)       ->  p "T-ABS         : ";
            let ctx'    = addbind ctx x (BindTmVar(tyT1)) in    (*          Γ,x:T1 ⊢ t2 : T2                *)  
            let tyT2    = typeof ctx' t2 in                     (*       --------------------- T-Abs        *)
            TyArr(tyT1,tyShift(-1)tyT2)                         (*       Γ ⊢ λx:T1.t2 : T1→T2               *)
    | TmApp(fi,t1,t2)           ->  p "T-APP         : ";     
            let tyT1 = typeof ctx t1 in                         (*   Γ |- t1 : T2→T12 ∧ Γ ⊢ t2 : T2         *)
            let tyT2 = typeof ctx t2 in                         (*   ------------------------------- T-App  *)
            (match simplifyty ctx tyT1 with                     (*         Γ ⊢ t1 t2 : T12                  *)   
                | TyArr(tyT11,tyT12)    -> if (tyeqv ctx) tyT2 tyT11 then tyT12 else error fi "type mismatch" 
                | _                     -> error fi "arrow type expected" ) *)
    | TmAbs(x,tyX,t)                ->  let t',tyT' = addTy_expr cns cname (add_var ctx x tyX) t in 
                                        TmAbs(x,tyX,(t',tyT')), TyAbs(tyX,tyT')
    | TmIdx(i,n)                    ->  begin match ctx with BdCtx local :: _ -> 
                                        let BdTy(id,ty) = L.nth local i in
                                        let ty = tyShift (i+1) ty  in 
                                        TmIdx(i,n)      , ty end 
    | TmReturn(r,c)                 ->  addTy_return      cns cname ctx  r c 
    | TmAbort                       ->  TmAbort         , TyVoid
    | TmLog(nm,args,_)              ->  let tyArgs      = L.map (addTy_expr cns cname ctx) args in
                                        let TyEv(id,tyEvArgs) = lookup_evnt nm ctx            in
                                        let tys         = L.map ty_of_var ( args_of_evnt_args tyEvArgs)    in
                                        assert(typechecks tys tyArgs) ;
                                        TmLog(nm,tyArgs,Some(TyEv(id,tyEvArgs))), TyTuple[]
    | TmSlfDstrct e                 ->  let e       = addTy_expr        cns cname ctx  e        in
                                        TmSlfDstrct e   , TyTuple[]
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
    | TmId     s                    ->  id_lookup_ty ctx s
    | EpBalance   e                 ->  let e       =   addTy_expr cns cname ctx e          in
                                        assert (tyeqv TyAddr (get_ty e));
                                        EpBalance e     , TyUint256
    | EpNew       n                 ->  let n,nm    =   addTy_new cns cname ctx n           in
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
                                        let idx,ty  =   addTy_expr cns cname ctx a.arrIdx in 
                                        assert (tyeqv kT ty) ; 
                                        EpArray { arrId    = e
                                                ; arrIdx = idx,ty }, vT end  
    | EpSend sd                     ->  let msg     =   addTy_expr  cns cname ctx sd.sd_msg in
                                        let cn      =   addTy_expr cns cname ctx sd.sd_cn   in
                                        begin match sd.sd_mthd with
                                        | Some m ->  
                                        let TyMthd(_,_,tyRet) = find_tyMthd m (L.map get_ty (typeof_cns cns)) in            
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

and addTy_new cns cname ctx e =
    let msg'        =   addTy_expr cns cname ctx e.new_msg in
    let args'       =   L.map (addTy_expr cns cname ctx) e.new_args in
    { new_id        =   e.new_id
    ; new_args      =   args'
    ; new_msg       =   msg'          }, e.new_id 

and addTy_lexpr cns cname ctx (LEpArray aa) = 
    let e = addTy_expr cns cname ctx aa.arrId in
    match get_ty e with
    | TyMap (kT,vT)     ->  let idx,ty = addTy_expr cns cname ctx aa.arrIdx in
                            LEpArray { arrId=e; arrIdx=idx,ty }  

and addTy_return cns cname ctx ret cont=
    let retTy       =   addTy_expr cns cname ctx ret    in
    let contTy      =   addTy_expr cns cname ctx cont   in 
    let tyRet       =   lookup_retTy ctx in 
    assert (tyeqv tyRet (get_ty retTy)); 
    TmReturn(retTy, contTy), get_ty retTy


and addTy_decl cns cname ctx (ty,id,v) =
    let v           =   addTy_expr cns cname ctx v in
    assert (is_known_ty (typeof_cns cns) ty);
    SmDecl(ty,id,v), add_var ctx id ty  

and addTy_stmt cns cname ctx = function 
    | SmIf(b,t,f)       ->  let b       = addTy_expr        cns cname ctx  b        in
                            let t       = addTy_stmts       cns cname ctx  t        in
                            let f       = addTy_stmts       cns cname ctx  f        in
                            SmIf(b,t,f)     , ctx 
    | SmAssign(l,r)     ->  let l       = addTy_lexpr       cns cname ctx  l        in
                            let r       = addTy_expr        cns cname ctx  r        in
                            SmAssign(l,r)   , ctx
    | SmDecl(ty,id,v)   ->                addTy_decl        cns cname ctx (ty,id,v) 
    | SmExpr e          ->  let e       = addTy_expr        cns cname ctx  e        in
                            (* assert(get_ty e=TyTuple[]) ; *) 
                            SmExpr e        , ctx

and addTy_stmts cns cname ctx = function 
    | []                ->  []
    | stmt::rest        ->  let stmt,ctx = addTy_stmt cns cname ctx stmt in
                            stmt :: addTy_stmts cns cname ctx rest






(* AssignTy Method / Contract / Toplevel  *) 
(* Default Method Returns Unit(==EmptyTuple) is a specification *) 
let retTy_of_mthd = function 
    | TyMthd(_,_,retTy)   -> retTy
    | TyDefault             -> TyTuple[]

let addTy_mthd cns cn_name ctx (TmMthd(head,body)) = 
    let retTy       =   retTy_of_mthd  head             in
    let argTys      =   argTys_of_mthd head             in
    let binds       =   binds_of_tys argTys             in
    let ctx'        =   add_retTy ctx retTy             in
    let ctx''       =   add_local ctx' binds            in
    TmMthd(addTy_mthd_head cns head, addTy_stmts cns cn_name ctx'' body)

let has_distinct_sigs (cn:unit cntrct) =
    let sigs        =   L.map (function 
                              | TmMthd(TyDefault,_)   -> None
                              | TmMthd(tyM,_)         -> Some (string_of_tyMthd tyM)) cn.mthds in
    let uniq_sigs   =   BL.unique sigs in
    L.length sigs=L.length uniq_sigs

let has_distinct_cntrct_names (cns : unit cntrct idxlist) : bool =
    let cn_names    = (L.map(fun(_,cn)->cn.id)cns) in
    L.length cns=L.length(BL.unique cn_names)

let addTy_cntrct cns (evs:ty idxlist) cn =
    assert (BL.for_all(arg_has_known_ty (typeof_cns cns))cn.fields  && has_distinct_sigs cn)  ; 
    let ctx         =   add_local (add_evnts empty_ctx(values evs)) (binds_of_tys cn.fields) in
    { id            =   cn.id
    ; fields        =   cn.fields 
    ; mthds         =   L.map(addTy_mthd cns cn.id ctx)cn.mthds }

let addTy_toplevel cns (evs:ty idxlist) = function 
    | Cntrct c      -> Cntrct (addTy_cntrct cns evs c)
    | Event  e      -> Event e

let addTys (tops : unit toplevel idxlist) : ty toplevel idxlist =
    let cntrcts                 = filter_map (function  | Cntrct c -> Some c
                                                        | _        -> None   ) tops in
    assert(has_distinct_cntrct_names(cntrcts));
    let tycns                   = map typeof_cn cntrcts in
    let evs : ty idxlist   = filter_map (function  | Event e  -> Some e
                                                        | _        -> None   ) tops in
    map (addTy_toplevel cntrcts evs) tops
