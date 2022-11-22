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
                                | TyAdr, TyIsc _    -> true
                                | _     , _             -> false ) 

let assert_tyeqv l r            =   let tyl = get_ty l in
                                    let tyr = get_ty r in 
                                    (* #DEBUG pf "asserting %s(typeof %s)=%s(typeof %s)\n"(str_of_ty tyl)(str_of_tm l)(str_of_ty tyr )(str_of_tm r); *)
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

let find_tyMthd_in_cn mname = find_by (function TyMthd(i,r,m)when i=mname    -> TyMthd(i,r,m)                    | _ -> raise Not_found) 
let find_tyMthd       mname = find_by (function TyCn(_,_,mthds)              -> find_tyMthd_in_cn mname mthds    | _ -> raise Not_found)

let typeof_mthd                 =   function 
    | TmMthd(TyMthd(id,ags,r),_)    ->  TyMthd(id, tys_of_vars ags, r)
    | TmMthd(TyDflt,_)           ->  TyMthd("", [], TyUnit   ) 

let typeof_cn(TmCn(id,flds,ms)) =   TyCn(id, tys_of_vars flds, L.map typeof_mthd ms)
let typeof_cns                  =   map typeof_cn 

let tycn_has_name     nm        =   function   TyCn(id,_,_) -> id=nm    | _ -> err"tycn_has_name"    
let cn_has_name       nm        =   function _,TyCn(id,_,_) -> id=nm    | _ -> err"cn_has_name" 
let is_known_cn tycns nm        =   BL.exists (cn_has_name nm) tycns

let rec is_known_ty tycns       =   function 
    | TyBytes32 | TyAdr            ->  true
    | TyU256 | TyU8 | TyBool        ->  true
    | TyUnit | TyErr                ->  true
    | TyTuple l                     ->  BL.for_all (is_known_ty tycns) l
    | TyRef l                       ->  is_known_ty tycns l
    | TyMap(a,b)                    ->  is_known_ty tycns a && is_known_ty tycns b
    | TyIsc cn                   ->  is_known_cn tycns cn

let arg_has_known_ty tycns      =   function 
    | TyVar(_,ty)                  ->  is_known_ty tycns ty 


(************************************) 
(**          EXPR  TyCheck         **) 
(************************************) 

let rec(-|???)tms (ctx,cns,cnm) = type_tms       cns cnm ctx tms  
and    (-|?)  tm  (ctx,cns,cnm) = type_tm        cns cnm ctx tm  
and    (-|??)(l,r)(ctx,cns,cnm) = type_binop_arg cns cnm ctx l r 
and  type_tms cns cname ctx es  = L.map (type_tm cns cname ctx) es
and  type_tm  cns cname ctx tm  = (* #DEBUG pe("type_tm: " ^ str_of_tm tm ); *)
    match fst tm with
    | TmApp(t1,t2)                  ->  let t1,ty1          =   t1      -|?  (ctx,cns,cname) in 
                                        let t2,ty2          =   t2      -|?  (ctx,cns,cname) in 
                                        begin match t1,ty1 with 
                                        | _,TyAbs(a,b) when tyeqv ty2 a -> TmApp((t1,ty1),(t2,ty2)), b
                                     (* | TmFix    _ , ty               -> TmApp((t1,tyT1),(t2,tyT2)), tyT *) 
                                        | TmIRec _          , ty        -> TmApp((t1,ty1),(t2,ty2)), ty
                                        | t                             -> pe_tm t; err "|- TmApp(TmAbs,_) : ??" end 
    | TmAbs(x,tyX,t)                ->  let ctx             =   (x,tyX) @@ ctx                          in 
                                        let t,tyT           =   t       -|?  (ctx,cns,cname)            in 
                                        TmAbs(x,tyX,(t,tyT)), TyAbs(tyX,tyT)
    | TmFix(f,(*,rig #TODO*)n,tyF,t)->  let TyAbs(tyN,tyR)  =   tyF                                     in 
                                        let ctx             =   (n,tyN) @@ (f,(*rig #TODO*)tyF) @@ ctx  in 
                                        let t,tyT           =   t       -|?  (ctx,cns,cname)            in 
                                        assert(subtype tyR tyT); 
                                        TmFix(f,n,tyF,(t,tyT)), tyF 
    | TmI(i,n)                      ->  begin match ctx with 
                                        | BdFrm lctx :: _   ->  let BdTy(id,ty) = L.nth lctx i          in
                                                                TmI(i,n)      , tyShift(i+1)ty  
                                        | _ :: ctx          ->  tm    -|?  (ctx,cns,cname)         
                                        | _                 ->  err "|- TmI : ??"                       end 
    | TmIRec(i(*, rig #TODO*) )     ->  begin match ctx with 
                                        | BdFrm lctx :: _   ->  let BdTy(id,ty) = L.nth lctx i          in 
                                                                TmIRec(i)     , tyShift(i+1)ty  
                                        | _ :: ctx          ->  tm    -|?  (ctx,cns,cname)         
                                        | _                 ->  err "|- TmIRec : ??"                    end 
    | TmIStrct(i)                   ->  begin match ctx with 
                                        | BdFrm lctx :: _   ->  let BdTy(id,ty) = L.nth lctx i          in 
                                                                TmIStrct(i)   , tyShift(i+1)ty  
                                        | _ :: ctx          ->  tm    -|?  (ctx,cns,cname)         
                                        | _                 ->  err "|- TmIStrct : ??"                  end 
    | TmIf(b,t1,t2)                 ->  let b,tyB           =   b       -|?  (ctx,cns,cname)             in 
                                        let t1,t2           =   (t1,t2) -|?? (ctx,cns,cname)             in 
                                        assert(tyeqv tyB TyBool); 
                                        TmIf((b,tyB),t1,t2) ,   get_ty t1 
    | TmSend(cn,Some m,args,msg)    ->  let msg             =   msg     -|?  (ctx,cns,cname)            in
                                        let cn              =   cn      -|?  (ctx,cns,cname)            in
                                        let args            =   args    -|???(ctx,cns,cname)             in
                                        ( match find_tyMthd m (L.map get_ty (typeof_cns cns)) with 
                                        | TyMthd(_,_,TyUnit)->  TmSend(cn,Some m,args,msg), TyRef TyUnit
                                        | TyMthd(_,_,r)     ->  TmDeref(TmSend(cn,Some m,args,msg),r),r)   (* #TODO this line should migrate to eval.ml *) 
    | TmSend(cn,None,args,msg)      ->  TmSend(cn -|?(ctx,cns,cname),None,[], msg -|?(ctx,cns,cname)), TyUnit 
    | TmId     s                    ->  id_lookup_ty ctx s
    | TmRet(r,c)                 ->  type_return  cns cname ctx  r c 
    | TmCall(id,args)               ->  type_call    cns cname ctx (TmCall(id,args))          
    | TmNew(id,args,msg)            ->  type_new     cns cname ctx id args msg    
    | TmLog(nm,args,_)              ->  let tyArgs          =   args   -|???  (ctx,cns,cname)             in
                                        let TyEv(id,tyev)   =   lookup_evnt nm ctx                      in
                                        let tys             =   tys_of_vars(args_of_ev_args tyev)       in
                                        assert(typechecks tys tyArgs) ;
                                        TmLog(nm,tyArgs,Some(TyEv(id,tyev))), TyUnit   
    | TmArr(a,i)                    ->  let a,TyMap(k,v)    =   a     -|?  (ctx,cns,cname)             in
                                        let i,ty            =   i     -|?  (ctx,cns,cname)             in 
                                        assert (tyeqv k ty) ; 
                                        TmArr((a,TyMap(k,v)),(i,ty)) , v   
    | Balanc      e                 ->  let e,ty            =   e       -|?  (ctx,cns,cname)             in
                                        assert (tyeqv TyAdr ty) ; 
                                        Balanc(e,ty)        ,   TyU256
    | TmLAND (l, r)                 ->  let l,r             =   (l,r)   -|?? (ctx,cns,cname)             in
                                        assert (tyeqv TyBool (get_ty r)); 
                                        TmLAND(l,r)         ,   TyBool
    | TmNOT  e                      ->  let e,ty            =   e       -|?  (ctx,cns,cname)             in
                                        assert (tyeqv TyBool ty) ; 
                                        TmNOT(e,ty)         ,   TyBool
    | TmLT (l, r)                   ->  let l,r = (l,r)  -|?? (ctx,cns,cname) in TmLT (l,r)  , TyBool 
    | TmGT (l, r)                   ->  let l,r = (l,r)  -|?? (ctx,cns,cname) in TmGT (l,r)  , TyBool 
    | TmEQ (l, r)                   ->  let l,r = (l,r)  -|?? (ctx,cns,cname) in TmEQ (l,r)  , TyBool 
    | TmNEQ(l, r)                   ->  let l,r = (l,r)  -|?? (ctx,cns,cname) in TmNEQ(l,r)  , TyBool 
    | TmMul(l, r)                   ->  let l,r = (l,r)  -|?? (ctx,cns,cname) in TmMul(l,r)  , get_ty l 
    | TmSub(l, r)                   ->  let l,r = (l,r)  -|?? (ctx,cns,cname) in TmSub(l,r)  , get_ty l
    | TmAdd(l, r)                   ->  let l,r = (l,r)  -|?? (ctx,cns,cname) in TmAdd(l,r)  , get_ty l 
    | TmSfDstr    e                 ->  TmSfDstr   (e    -|?  (ctx,cns,cname))              , TyUnit           
    | TmAddr      e                 ->  TmAddr     (e    -|?  (ctx,cns,cname))              , TyAdr  
    | TmU256      d                 ->  TmU256      d                                       , TyU256
    | TmU8        d                 ->  TmU8        d                                       , TyU8
    | TmAbort                       ->  TmAbort                                             , TyErr
    | TmUnit                        ->  TmUnit                                              , TyUnit    
    | TmTrue                        ->  TmTrue                                              , TyBool
    | TmFalse                       ->  TmFalse                                             , TyBool
    | TmSender                      ->  TmSender                                            , TyAdr
    | EpNow                         ->  EpNow                                               , TyU256
    | TmThis                        ->  TmThis                                              , TyIsc cname
    | EpValue                       ->  EpValue                                             , TyU256
    | TmAsgn((TmArr(a,i),_),r)    ->  let a,TyMap(k,v)    =   a       -|?  (ctx,cns,cname)             in  
                                        let i               =   i       -|?  (ctx,cns,cname)             in 
                                        let r               =   r       -|?  (ctx,cns,cname)             in 
                                        TmAsgn((TmArr((a,TyMap(k,v)),i),TyRef v),r)     , TyUnit 

and type_binop_arg cns cname ctx l r = 
    let l               =   l       -|?  (ctx,cns,cname)     in 
    let r               =   r       -|?  (ctx,cns,cname)     in 
    assert_tyeqv l r; 
    l,r 

and type_new cns cname ctx id args msg =
    let msg             =   msg     -|?  (ctx,cns,cname)     in
    let args            =   args    -|???(ctx,cns,cname)     in
    TmNew(id,args,msg)  ,   TyIsc id 

and type_return cns cname ctx ret cont=
    let ret             =   ret     -|?  (ctx,cns,cname)     in
    let cont            =   cont    -|?  (ctx,cns,cname)     in
    let rety            =   lookup_rety ctx in 
    assert (tyeqv rety (get_ty ret)); 
    TmRet(ret,cont)  ,   rety

and check_call_arg_types tycns  =   function 
    | "pre_ecdsarecover"            ->  (=) [TyBytes32;TyU8;TyBytes32;TyBytes32]
    | "keccak256"                   ->  konst true
    | "iszero"                      ->  fun x ->  x=[TyBytes32]||x=[TyU8]||x=[TyU256]||x=[TyBool]||x=[TyAdr]
    | name                          ->  let cnidx               = lookup_idx (tycn_has_name name) tycns in
                                        let TyCn(_,tyargs,_)    = lookup cnidx tycns                    in 
                                        (=) tyargs 

and check_args_match tycns args =   function 
    | Some m                        ->  assert (check_call_arg_types tycns m (L.map get_ty args))
    | None                          ->  assert (isNil (L.map get_ty args))

and type_call cns cname ctx (TmCall(id,args)) =
    let args        = type_tms cns cname ctx args in
    check_args_match (typeof_cns cns) args (Some id) ; 
    let rety        = match id with
        | "value" when true         ->  TyU256 (* check the arg is 'msg' *) 
        | "pre_ecdsarecover"        ->  TyAdr
        | "keccak256"               ->  TyBytes32
        | "iszero"                  ->  begin match args with
            | [arg]                     ->  TyBool
            | _                         ->  err "should not happen" end 
        | cnname when true          ->  let i,cn = lookup_icn cns cnname in 
                                        typeof_cn cn 
        | _                         ->  err "type_call: should not happen" in
    TmCall(id,args) , rety 

and typechecks tys actual       =   L.for_all2 (fun ty (_,a)-> ty=a) tys actual


(************************************) 
(**         METHOD TyCheck         **) 
(************************************) 

let type_mthd_head cns         =   function 
    | TyDflt                     ->  TyDflt
    | TyMthd(id,argTys,rety)       ->  assert (BL.for_all (arg_has_known_ty (typeof_cns cns)) argTys) ; 
                                        (* #DEBUG  pe("is_known_ty: " ^ str_of_ty rety);  *)
                                        assert (is_known_ty (typeof_cns cns) rety) ;
                                        TyMthd(id,argTys,rety)

let rettypeof_mthd = function 
    | TyMthd(_,_,rety)      -> rety
    | TyDflt             -> TyUnit   

let type_mthd cns cn_name ctx (TmMthd(head,body)) = 
    let rety        =   rettypeof_mthd head             in
    let argTys      =   argTys_of_mthd head             in
    let binds       =   binds_of_tys argTys             in
    let ctx'        =   add_rety ctx rety              in
    let ctx''       =   add_local ctx' binds            in
    TmMthd(type_mthd_head cns head, type_tm cns cn_name ctx'' body)

let unique_sig (TmCn(_,_,mthds)) =
    let sigs        =   L.map (function | TmMthd(TyDflt,_)   -> None
                                        | TmMthd(tyM,_)         -> Some (str_of_tyMthd tyM)) mthds in
    L.length sigs = L.length(BL.unique sigs) 

let unique_cn cns =
    let cnames      =   L.map(function _,TmCn(id,_,_) -> id) cns in
    L.length cns  = L.length(BL.unique cnames)

let type_cntrct cns (evs:ty ilist) (TmCn(id,flds,mthds)) = 
    assert (BL.for_all(arg_has_known_ty (typeof_cns cns)) flds) ; 
    assert (unique_sig (TmCn(id,flds,mthds)))  ; 
    let ctx         =   add_local (add_evnts empty_ctx(values evs)) (binds_of_tys flds) in
    TmCn(id,flds,L.map(type_mthd cns id ctx) mthds) 

let type_top cns (evs:ty ilist) = function 
    | TmEv e      -> TmEv e
    | t           -> type_cntrct cns evs t

let type_tops (tops : unit toplevel ilist)  =
    let cns         = filter_map (function  | TmEv e   -> None 
                                            | c        -> Some c ) tops in
    assert(unique_cn cns);
    let evs         = filter_map (function  | TmEv e   -> Some e
                                            | _        -> None   ) tops in
    map (type_top cns evs) tops
