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
(***      Type Equivalence       ***)
(***********************************)

let tyeqv t0 t1                 =   ( t0 = t1 )  ||  ( match t0, t1 with
                                | TyErr, _ | _, TyErr -> true
                                | TyAddr, TyInstnc _    -> true
                                | _     , _             -> false ) 

let assert_tyeqv l r            =   let tyl = get_ty l in
                                    let tyr = get_ty r in 
                                    printf "asserting %s(typeof %s)=%s(typeof %s)\n" (str_of_ty tyl) (str_of_tm l)  (str_of_ty tyr ) (str_of_tm r); 
                                    assert (tyeqv tyl tyr) 
                                
let id_lookup_ty ctx id         =   try TmId id, lookup_id id ctx 
                                    with Not_found -> err("unknown id "^id)  

(************************************) 
(**           SubTyping            **) 
(************************************) 

let subtype                     = tyeqv 



(************************************) 
(**         METHOD TyCheck         **) 
(************************************) 

let find_tyMthd_in_cn mname = find_by_filter (function TyMthd(i,r,m)when i=mname    -> TyMthd(i,r,m)                    | _ -> raise Not_found) 
let find_tyMthd       mname = find_by_filter (function TyCn(_,_,mthds)              -> find_tyMthd_in_cn mname mthds    | _ -> raise Not_found)

let typeof_mthd                 =   function 
    | TmMthd(TyMthd(id,ags,r),_)    ->  TyMthd(id, tys_of_vars ags, r)
    | TmMthd(TyDefault,_)           ->  TyMthd("", [], TyUnit   ) 

let typeof_cn(TmCn(id,flds,ms)) =   TyCn(id, tys_of_vars flds, L.map typeof_mthd ms)
let typeof_cns                  =   map typeof_cn 

let tycn_has_name     nm        =   function   TyCn(id,_,_) -> id=nm    | _ -> err"tycn_has_name"    
let cn_has_name       nm        =   function _,TyCn(id,_,_) -> id=nm    | _ -> err"cn_has_name" 
let is_known_cn tycns nm        =   BL.exists (cn_has_name nm) tycns

let rec is_known_ty tycns       =   function 
    | TyBytes32 | TyAddr            ->  true
    | TyU256 | TyU8 | TyBool        ->  true
    | TyUnit | TyErr               ->  true
    | TyTuple l                     ->  BL.for_all (is_known_ty tycns) l
    | TyRef l                       ->  is_known_ty tycns l
    | TyMap(a,b)                    ->  is_known_ty tycns a && is_known_ty tycns b
    | TyInstnc cn                   ->  is_known_cn tycns cn

let arg_has_known_ty tycns      =   function 
    | TyVar(_,ty)                  ->  is_known_ty tycns ty 


(************************************) 
(**          EXPR  TyCheck         **) 
(************************************) 

let rec (--|)  tms (ctx,cns,cnm)  = addTy_tms     cns cnm ctx tms  
and     (-|)    tm (ctx,cns,cnm)  = addTy_tm      cns cnm ctx tm  
and     (-||)  (l,r) (ctx,cns,cnm)  = addTy_binop_arg cns cnm ctx l r 
and  addTy_tms cns cname ctx es   = L.map (addTy_tm cns cname ctx) es
and  addTy_tm  cns cname ctx tm = pe("addTy_tm: " ^ str_of_tm tm );match fst tm with
    | TmApp(t1,t2)                  ->  let t1,ty1          =   t1      -|  (ctx,cns,cname) in 
                                        let t2,ty2          =   t2      -|  (ctx,cns,cname) in 
                                        begin match t1,ty1 with 
                                        | _,TyAbs(a,b) when tyeqv ty2 a -> TmApp((t1,ty1),(t2,ty2)), b
                                     (* | TmFix    _ , ty               -> TmApp((t1,tyT1),(t2,tyT2)), tyT *) 
                                        | TmIRec _ , ty               -> TmApp((t1,ty1),(t2,ty2)), ty
                                        | t                             -> pe_tm t; err "|- TmApp(TmAbs,_) : ??" end 
    | TmAbs(x,tyX,t)                ->  let ctx             =   (x,tyX) @@ ctx                          in 
                                        let t,tyT           =   t       -|  (ctx,cns,cname)             in 
                                        TmAbs(x,tyX,(t,tyT)), TyAbs(tyX,tyT)
    | TmFix(f,(*,rig #TODO*)n,tyF,t)->  let TyAbs(tyN,tyR)  =   tyF                                     in 
                                        let ctx            =   (n,tyN) @@ (f,(*rig #TODO*)tyF) @@ ctx   in 
                                        let t,tyT           =   t       -|  (ctx,cns,cname)             in 
                                        assert(subtype tyR tyT); 
                                        TmFix(f,n,tyF,(t,tyT)), tyF 
    | TmI(i,n)                      ->  begin match ctx with 
                                        | BdCtx lctx :: _   ->  let BdTy(id,ty) = L.nth lctx i          in
                                                                TmI(i,n)      , tyShift(i+1)ty  
                                        | _ :: ctx          ->  tm    -|  (ctx,cns,cname)         
                                        | _                 ->  err "|- TmI : ??"                     end 
    | TmIRec(i(*, rig #TODO*) )     ->  begin match ctx with 
                                        | BdCtx lctx :: _   ->  let BdTy(id,ty) = L.nth lctx i          in 
                                                                TmIRec(i)     , tyShift(i+1)ty  
                                        | _ :: ctx          ->  tm    -|  (ctx,cns,cname)         
                                        | _                 ->  err "|- TmIRec : ??"                  end 
    | TmIStrct(i)                   ->  begin match ctx with 
                                        | BdCtx lctx :: _   ->  let BdTy(id,ty) = L.nth lctx i          in 
                                                                TmIStrct(i)   , tyShift(i+1)ty  
                                        | _ :: ctx          ->  tm    -|  (ctx,cns,cname)         
                                        | _                 ->  err "|- TmIStrct : ??"                end 
    | TmIf(b,t1,t2)                 ->  let b,tyB           =   b       -|  (ctx,cns,cname)             in 
                                        let t1,t2           =   (t1,t2) -|| (ctx,cns,cname)             in 
                                        assert(tyeqv tyB TyBool); 
                                        TmIf((b,tyB),t1,t2) ,   get_ty t1 
    | TmSend(cn,Some m,args,msg)    ->  let msg             =   msg     -|  (ctx,cns,cname)             in
                                        let cn              =   cn      -|  (ctx,cns,cname)             in
                                        let args            =   args   --|  (ctx,cns,cname)             in
                                        ( match find_tyMthd m (L.map get_ty (typeof_cns cns)) with 
                                        | TyMthd(_,_,TyUnit)->  TmSend(cn,Some m,args,msg), TyRef TyUnit
                                        | TyMthd(_,_,r)     ->  TmDeref(TmSend(cn,Some m,args,msg),r),r )         (* #TODO this line should migrate to eval.ml *) 
    | TmSend(cn,None,args,msg)      ->  TmSend(cn -|(ctx,cns,cname),None,[], msg -|(ctx,cns,cname)), TyUnit 
    | TmId     s                    ->  id_lookup_ty ctx s
    | TmReturn(r,c)                 ->  addTy_return  cns cname ctx  r c 
    | TmCall(id,args)               ->  addTy_call    cns cname ctx (TmCall(id,args))          
    | TmNew(id,args,msg)            ->  addTy_new     cns cname ctx id args msg    
    | TmLog(nm,args,_)              ->  let tyArgs          =   args   --|  (ctx,cns,cname)             in
                                        let TyEv(id,tyev)   =   lookup_evnt nm ctx                      in
                                        let tys             =   tys_of_vars(args_of_ev_args tyev)       in
                                        assert(typechecks tys tyArgs) ;
                                        TmLog(nm,tyArgs,Some(TyEv(id,tyev))), TyUnit   
    | TmArray(aid,aidx)             ->  let a,TyMap(k,v)    =   aid     -|  (ctx,cns,cname)             in
                                        let i,ty            =   aidx    -|  (ctx,cns,cname)             in 
                                        assert (tyeqv k ty) ; 
                                        TmArray((a,TyMap(k,v)),(i,ty)) , v   
    | Balanc      e                 ->  let e               =   e       -|  (ctx,cns,cname)             in
                                        assert (tyeqv TyAddr (get_ty e)) ; 
                                        Balanc e            ,   TyU256
    | TmLAND (l, r)                 ->  let l,r             =   (l,r)   -|| (ctx,cns,cname)             in
                                        assert (tyeqv TyBool (get_ty r)); 
                                        TmLAND(l,r)         ,   TyBool
    | TmNOT  e                      ->  let e               =   e       -|  (ctx,cns,cname)             in
                                        assert (tyeqv TyBool (get_ty e)) ; 
                                        TmNOT e             ,   TyBool
    | TmLT (l, r)                   ->  let l,r = (l,r)  -|| (ctx,cns,cname) in TmLT (l,r)  , TyBool 
    | TmGT (l, r)                   ->  let l,r = (l,r)  -|| (ctx,cns,cname) in TmGT (l,r)  , TyBool 
    | TmEQ (l, r)                   ->  let l,r = (l,r)  -|| (ctx,cns,cname) in TmEQ (l,r)  , TyBool 
    | TmNEQ(l, r)                   ->  let l,r = (l,r)  -|| (ctx,cns,cname) in TmNEQ(l,r)  , TyBool 
    | TmMul(l, r)                   ->  let l,r = (l,r)  -|| (ctx,cns,cname) in TmMul(l,r)  , get_ty l 
    | TmSub(l, r)                   ->  let l,r = (l,r)  -|| (ctx,cns,cname) in TmSub(l,r)  , get_ty l
    | TmAdd(l, r)                   ->  let l,r = (l,r)  -|| (ctx,cns,cname) in TmAdd(l,r)  , get_ty l 
    | TmSfDstr e                 ->  TmSfDstr(e    -|  (ctx,cns,cname))               , TyUnit           
    | EpAddr      e                 ->  EpAddr     (e    -|  (ctx,cns,cname))               , TyAddr  
    | TmU256      d                 ->  TmU256      d                                       , TyU256
    | TmU8        d                 ->  TmU8        d                                       , TyU8
    | TmAbort                       ->  TmAbort                                             , TyErr
    | TmUnit                        ->  TmUnit                                              , TyUnit    
    | TmTrue                        ->  TmTrue                                              , TyBool
    | TmFalse                       ->  TmFalse                                             , TyBool
    | EpSender                      ->  EpSender                                            , TyAddr
    | EpNow                         ->  EpNow                                               , TyU256
    | EpThis                        ->  EpThis                                              , TyInstnc cname
    | EpValue                       ->  EpValue                                             , TyU256
    | TmAssign((TmArray(a,i),_),r)  ->  let a,TyMap(k,v)    =   a       -|  (ctx,cns,cname)             in  
                                        let i               =   i       -|  (ctx,cns,cname)             in 
                                        let r               =   r       -|  (ctx,cns,cname)             in 
                                        TmAssign((TmArray((a,TyMap(k,v)),i),TyRef v),r)     , TyUnit 

and addTy_binop_arg cns cname ctx l r = 
    let l               =   l       -|  (ctx,cns,cname)     in 
    let r               =   r       -|  (ctx,cns,cname)     in 
    assert_tyeqv l r; 
    l,r 

and addTy_new cns cname ctx id args msg =
    let msg             =   msg     -|  (ctx,cns,cname)     in
    let args            =   args   --|  (ctx,cns,cname)     in
    TmNew(id,args,msg)  ,   TyInstnc id 

and addTy_return cns cname ctx ret cont=
    let ret             =   ret     -|  (ctx,cns,cname)     in
    let cont            =   cont    -|  (ctx,cns,cname)     in
    let rety            =   lookup_retTy ctx in 
    assert (tyeqv rety (get_ty ret)); 
    TmReturn(ret,cont)  ,   rety

and check_call_arg_types tycns  =   function 
    | "pre_ecdsarecover"            ->  (=) [TyBytes32;TyU8;TyBytes32;TyBytes32]
    | "keccak256"                   ->  konst true
    | "iszero"                      ->  fun x ->  x=[TyBytes32]||x=[TyU8]||x=[TyU256]||x=[TyBool]||x=[TyAddr]
    | name                          ->  let cnidx               = lookup_idx (tycn_has_name name) tycns in
                                        let TyCn(_,tyargs,_)    = lookup cnidx tycns                    in 
                                        (=) tyargs 

and check_args_match tycns args =   function 
    | Some m                        ->  assert (check_call_arg_types tycns m (L.map get_ty args))
    | None                          ->  assert (isNil (L.map get_ty args))

and addTy_call cns cname ctx (TmCall(id,args)) =
    let args        = addTy_tms cns cname ctx args in
    check_args_match (typeof_cns cns) args (Some id) ; 
    let rety        = match id with
        | "value" when true         ->  TyU256 (* check the arg is 'msg' *) 
        | "pre_ecdsarecover"        ->  TyAddr
        | "keccak256"               ->  TyBytes32
        | "iszero"                  ->  begin match args with
            | [arg]                     ->  TyBool
            | _                         ->  err "should not happen" end 
        | cnname when true          ->  let i,cn = lookup_icn_of_icns cns cnname in 
                                        typeof_cn cn 
        | _                         ->  err "addTy_call: should not happen" in
    TmCall(id,args) , rety 

and typechecks tys actual       =   L.for_all2 (fun ty (_,a)-> ty=a) tys actual


(************************************) 
(**         METHOD TyCheck         **) 
(************************************) 

let addTy_mthd_head cns         =   function 
    | TyDefault                     ->  TyDefault
    | TyMthd(id,argTys,retTy)       ->  assert (BL.for_all (arg_has_known_ty (typeof_cns cns)) argTys) ; 
                                        pe("is_known_ty: " ^ str_of_ty retTy);  
                                        assert (is_known_ty (typeof_cns cns) retTy) ;
                                        TyMthd(id,argTys,retTy)

let rettypeof_mthd = function 
    | TyMthd(_,_,rety)      -> rety
    | TyDefault             -> TyUnit   

let addTy_mthd cns cn_name ctx (TmMthd(head,body)) = 
    let rety        =   rettypeof_mthd head             in
    let argTys      =   argTys_of_mthd head             in
    let binds       =   binds_of_tys argTys             in
    let ctx'        =   add_retTy ctx rety              in
    let ctx''       =   add_local ctx' binds            in
    TmMthd(addTy_mthd_head cns head, addTy_tm cns cn_name ctx'' body)

let unique_sig (TmCn(_,_,mthds)) =
    let sigs        =   L.map (function | TmMthd(TyDefault,_)   -> None
                                        | TmMthd(tyM,_)         -> Some (str_of_tyMthd tyM)) mthds in
    L.length sigs = L.length(BL.unique sigs) 

let unique_cn cns =
    let cnames      =   L.map(function _,TmCn(id,_,_) -> id) cns in
    L.length cns  = L.length(BL.unique cnames)

let addTy_cntrct cns (evs:ty idxlist) (TmCn(id,flds,mthds)) = 
    assert (BL.for_all(arg_has_known_ty (typeof_cns cns)) flds) ; 
    assert (unique_sig (TmCn(id,flds,mthds)))  ; 
    let ctx         =   add_local (add_evnts empty_ctx(values evs)) (binds_of_tys flds) in
    TmCn(id,flds,L.map(addTy_mthd cns id ctx) mthds) 

let addTy_toplevel cns (evs:ty idxlist) = function 
    | TmEv e      -> TmEv e
    | t           -> addTy_cntrct cns evs t

let addTys (tops : unit toplevel idxlist)  =
    let cns         = filter_map (function  | TmEv e   -> None 
                                            | c        -> Some c ) tops in
    assert(unique_cn cns);
    let evs         = filter_map (function  | TmEv e   -> Some e
                                            | _        -> None   ) tops in
    map (addTy_toplevel cns evs) tops
