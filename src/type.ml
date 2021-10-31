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
                                | TyAddr, TyInstnc _    -> true
                                | _     , _             -> false ) 

let assert_tyeqv l r            =   let tyl = get_ty l in
                                    let tyr = get_ty r in 
                                    printf "asserting %s(typeof %s)=%s(typeof %s)\n" (str_of_ty tyl) (str_of_tm l)  (str_of_ty tyr ) (str_of_tm r); 
                                    assert (tyl = tyr) 
                                
let typeof_mthd                 =   function 
    | TmMthd(TyMthd(id,args,ret),_) ->  TyMthd(id, tys_of_vars args, ret)
    | TmMthd(TyDefault,_)           ->  TyMthd("", [], TyUnit   ) 

let typeof_cn(TmCn(id,flds,ms)) =   TyCn(id, tys_of_vars flds, L.map typeof_mthd ms)
let typeof_cns                  =   map typeof_cn 

let id_lookup_ty ctx id         =   try TmId id, lookup_id id ctx 
                                    with Not_found -> err("unknown id "^id)  

(************************************) 
(**           SubTyping            **) 
(************************************) 

let subtype                     = tyeqv 




(************************************) 
(**         Arg Type Check         **) 
(************************************) 

let tycn_has_name  name         =   function TyCn(id,_,_) -> id=name     
let itycn_has_name name itycn   =   match get_ty itycn with TyCn(id,_,_) -> id=name 
let is_known_cntrct tycns nm    =   BL.exists (itycn_has_name nm) tycns

let rec is_known_ty tycns       =   function 
    | TyBytes32 | TyAddr | TyUnit   ->  true
    | TyU256 | TyU8 | TyBool        ->  true
    | TyTuple l                     ->  BL.for_all (is_known_ty tycns) l
    | TyRef l                       ->  is_known_ty tycns l
    | TyMap(a,b)                    ->  is_known_ty tycns a && is_known_ty tycns b
    | TyInstnc cn                   ->  is_known_cntrct tycns cn

let arg_has_known_ty tycns      =   function 
    | TyVar(id,ty)                  ->  if is_known_ty tycns ty 
                                            then true
                                            else err("Unknown Arg Type "^str_of_ty ty)

let addTy_mthd_head cns         =   function 
    | TyDefault                     ->  TyDefault
    | TyMthd(id,argTys,retTy)       ->  assert (BL.for_all (arg_has_known_ty (typeof_cns cns)) argTys) ; 
                                        assert (is_known_ty (typeof_cns cns) retTy) ;
                                        TyMthd(id,argTys,retTy)

let call_arg_expectations tycns =   function 
    | "pre_ecdsarecover"            ->  (=) [TyBytes32;TyU8;TyBytes32;TyBytes32]
    | "keccak256"                   ->  konst true
    | "iszero"                      ->  fun x -> x=[TyBytes32]||x=[TyU8]||x=[TyU256]||x=[TyBool]||x=[TyAddr]
    | name                          ->  let cn_idx       = lookup_idx (tycn_has_name name) tycns in
                                        match lookup cn_idx tycns with TyCn(_,tyCnArgs,_) -> 
                                        (=) tyCnArgs

let typecheck  (ty,(_,t))       =   assert (ty = t)
let typechecks tys actual       =   L.for_all2 (fun ty (_,a)-> ty=a) tys actual

let check_args_match tycns args =   function 
    | Some m                        ->  assert (call_arg_expectations tycns m (L.map get_ty args))
    | None                          ->  assert (isNil (L.map get_ty args))

let rec addTy_call cns cname ctx (TmCall(id,args)) =
    let args = L.map (addTy_expr cns cname ctx) args in
    check_args_match (typeof_cns cns) args (Some id) ; 
    let rety     = match id with
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


and addTy_expr cns cname ctx expr = pe("addTy_expr: " ^ str_of_tm expr );match fst expr with
    | TmApp(t1,t2)                  ->  let t1,ty1          = addTy_expr cns cname ctx t1 in 
                                        let t2,ty2          = addTy_expr cns cname ctx t2 in 
                                        begin match t1,ty1 with 
                                        | _,TyAbs(a,b) when tyeqv ty2 a -> TmApp((t1,ty1),(t2,ty2)), b
                                     (* | TmFix    _ , ty               -> TmApp((t1,tyT1),(t2,tyT2)), tyT *) 
                                        | TmIdxRec _ , ty               -> TmApp((t1,ty1),(t2,ty2)), ty
                                        | t                             -> pe(str_of_tm t); err"addTy_expr: T-APPABS failed" end 
    | TmAbs(x,tyX,t)                ->  let ctx             =   add_var ctx x tyX                       in 
                                        let t,tyT           =   addTy_expr cns cname ctx t              in 
                                        TmAbs(x,tyX,(t,tyT)), TyAbs(tyX,tyT)
    | TmFix(f,n,tyF,t)              ->  let TyAbs(tyN,tyR)  =   tyF                                     in 
                                        let ctx'            =   add_var (add_var ctx f tyF) n tyN       in 
                                        let t,tyT           =   addTy_expr cns cname ctx' t             in 
                                        assert(subtype tyR tyT); 
                                        TmFix(f,n,tyF,(t,tyT)), tyF 
    | TmIdx(i,n)                    ->  begin match ctx with 
                                        | BdCtx lctx :: _   ->  let BdTy(id,ty) = L.nth lctx i          in
                                                                TmIdx(i,n)      , tyShift(i+1)ty  
                                        | b :: bs           ->  addTy_expr cns cname bs expr
                                        | _                 ->  err "addTy_expr: TmIdx: Notfound"       end 
    | TmIdxRec(i)                   ->  begin match ctx with 
                                        | BdCtx lctx :: _   ->  let BdTy(id,ty) = L.nth lctx i          in 
                                                                TmIdxRec(i) , tyShift(i+1)ty  
                                        | b :: bs           ->  addTy_expr cns cname bs expr
                                        | _                 ->  err "addTy_expr: TmIdxRec: Not found"   end 
    | TmIdxStrct(i)                 ->  begin match ctx with 
                                        | BdCtx lctx :: _   ->  let BdTy(id,ty) = L.nth lctx i          in 
                                                                TmIdxStrct(i) , tyShift(i+1)ty  
                                        | b :: bs           ->  addTy_expr cns cname bs expr
                                        | _                 ->  err "addTy_expr: TmIdxStrct: Not found" end 
    | TmIf(b,t1,t2)                 ->  let b,tyB           =   addTy_expr cns cname ctx b              in 
                                        let t1,t2           =   addTy_binop_arg cns cname ctx t1 t2     in 
                                        assert(tyB = TyBool); 
                                        TmIf((b,tyB),t1,t2) ,   get_ty t1 
    | TmSend(cn,Some m,args,msg)    ->  let msg             =   addTy_expr cns cname ctx msg            in
                                        let cn              =   addTy_expr cns cname ctx cn             in
                                        let args            =   L.map(addTy_expr cns cname ctx)args     in                
                                        ( match find_tyMthd m (L.map get_ty (typeof_cns cns)) with 
                                        | TyMthd(_,_,TyUnit)->  TmSend(cn,Some m,args,msg), TyRef TyUnit
                                        | TyMthd(_,_,rety)  ->  EpDeref(TmSend(cn,Some m,args,msg),rety),rety )
    | TmSend(cn,None,args,msg)      ->  let msg             =   addTy_expr cns cname ctx msg            in
                                        let cn              =   addTy_expr cns cname ctx cn             in
                                        TmSend(cn,None,[],msg), TyUnit 
    | TmLog(nm,args,_)              ->  let tyArgs          =   L.map(addTy_expr cns cname ctx)args     in
                                        let TyEv(id,tyev)   =   lookup_evnt nm ctx                      in
                                        let tys             =   L.map ty_of_var(args_of_ev_args tyev)   in
                                        assert(typechecks tys tyArgs) ;
                                        TmLog(nm,tyArgs,Some(TyEv(id,tyev))), TyUnit   
    | TmArray(aid,aidx)             ->  let a,TyMap(k,v)    =   addTy_expr cns cname ctx aid            in
                                        let i,ty            =   addTy_expr cns cname ctx aidx           in 
                                        assert (tyeqv k ty) ; 
                                        TmArray((a,TyMap(k,v)),(i,ty)) , v   
    | TmSlfDstrct e                 ->  let e               =   addTy_expr cns cname ctx e              in
                                        TmSlfDstrct e       ,   TyUnit   
    | EpAddr      e                 ->  let e               =   addTy_expr cns cname ctx e              in
                                        EpAddr e            ,   TyAddr
    | Balanc      e                 ->  let e               =   addTy_expr cns cname ctx e              in
                                        assert (tyeqv TyAddr (get_ty e)) ; 
                                        Balanc e            ,   TyU256
    | TmId     s                    ->  id_lookup_ty ctx s
    | TmReturn(r,c)                 ->  addTy_return  cns cname ctx  r c 
    | TmCall(id,args)               ->  addTy_call    cns cname ctx (TmCall(id,args))          
    | TmNew(id,args,msg)            ->  addTy_new     cns cname ctx id args msg    
    | EpLAnd (l, r)                 ->  let l,r             =   addTy_binop_arg cns cname ctx l r       in
                                        typecheck (TyBool,r); 
                                        EpLAnd(l,r)         ,   TyBool
    | EpLT (l, r)                   ->  let l,r             =   addTy_binop_arg cns cname ctx l r       in
                                        EpLT  (l, r)        ,   TyBool
    | EpGT (l, r)                   ->  let l,r             =   addTy_binop_arg cns cname ctx l r       in
                                        EpGT  (l, r)        ,   TyBool
    | EpNEq (l, r)                  ->  let l,r             =   addTy_binop_arg cns cname ctx l r       in
                                        EpNEq (l, r)        ,   TyBool
    | TmEq (l, r)                   ->  let l,r             =   addTy_binop_arg cns cname ctx l r       in
                                        TmEq (l, r)         ,   TyBool
    | TmAdd (l, r)                  ->  let l,r             =   addTy_binop_arg cns cname ctx l r       in
                                        TmAdd (l, r)        ,   get_ty l
    | TmSub (l, r)                  ->  let l,r             =   addTy_binop_arg cns cname ctx l r       in
                                        TmSub (l, r)        ,   get_ty l
    | TmMul (l, r)                  ->  let l,r             =   addTy_binop_arg cns cname ctx l r       in
                                        TmMul (l, r)        ,   get_ty l
    | EpNot  e                      ->  let e               =   addTy_expr cns cname ctx e              in
                                        assert (get_ty e=TyBool) ; 
                                        EpNot e             ,   TyBool
    | TmAbort                       ->  TmAbort             ,   TyVoid
    | TmUnit                        ->  TmUnit              ,   TyUnit    
    | EpThis                        ->  EpThis              ,   TyInstnc cname
    | TmTrue                        ->  TmTrue              ,   TyBool
    | TmFalse                       ->  TmFalse             ,   TyBool
    | EpSender                      ->  EpSender            ,   TyAddr
    | EpNow                         ->  EpNow               ,   TyU256
    | TmU256      d                 ->  TmU256      d       ,   TyU256
    | TmU8        d                 ->  TmU8        d       ,   TyU8
    | EpValue                       ->  EpValue             ,   TyU256

and addTy_binop_arg cns cname ctx l r = 
    let l               =   addTy_expr cns cname ctx l    in 
    let r               =   addTy_expr cns cname ctx r    in
    assert_tyeqv l r; 
    l,r 

and addTy_new cns cname ctx id args msg =
    let msg             =   addTy_expr cns cname ctx msg in
    let args            =   L.map (addTy_expr cns cname ctx) args in
    TmNew(id,args,msg)  ,   TyInstnc id 

and addTy_lexpr cns cname ctx (TmArray(id,idx)) = 
    let s,TyMap(k,v)    =   addTy_expr cns cname ctx id     in 
    let idx             =   addTy_expr cns cname ctx idx    in 
    TmArray((s,TyMap(k,v)),idx) 

and addTy_return cns cname ctx ret cont=
    let ret             =   addTy_expr cns cname ctx ret    in
    let cont            =   addTy_expr cns cname ctx cont   in 
    let rety            =   lookup_retTy ctx in 
    assert (tyeqv rety (get_ty ret)); 
    TmReturn(ret,cont)  ,   rety



(*======= STMT ========*)

and addTy_stmt cns cname ctx = function 
    | SmIf(b,t,f)       ->  let b       = addTy_expr        cns cname ctx  b        in
                            let t       = addTy_stmts       cns cname ctx  t        in
                            let f       = addTy_stmts       cns cname ctx  f        in
                            SmIf(b,t,f)     , ctx 
    | SmAssign(l,r)     ->  let l       = addTy_lexpr       cns cname ctx  l        in
                            let r       = addTy_expr        cns cname ctx  r        in
                            SmAssign(l,r)   , ctx
    | SmDecl(ty,id,v)   ->  addTy_decl        cns cname ctx ty id v
    | SmExpr e          ->  let e       = addTy_expr        cns cname ctx  e        in
                            SmExpr e        , ctx

and addTy_stmts cns cname ctx = function 
    | []                ->  []
    | stmt::rest        ->  let stmt,ctx = addTy_stmt cns cname ctx stmt in
                            stmt :: addTy_stmts cns cname ctx rest

and addTy_decl cns cname ctx ty id v =
    let v               =   addTy_expr cns cname ctx v in
    assert (is_known_ty (typeof_cns cns) ty);
    SmDecl(ty,id,v)     ,   add_var ctx id ty  



(* AssignTy Method / Contract / Toplevel  *) 
(* Default Method Returns Unit(==EmptyTuple) is a specification *) 
let rettypeof_mthd = function 
    | TyMthd(_,_,rety)      -> rety
    | TyDefault             -> TyUnit   

let addTy_mthd cns cn_name ctx (TmMthd(head,body)) = 
    let rety        =   rettypeof_mthd head             in
    let argTys      =   argTys_of_mthd head             in
    let binds       =   binds_of_tys argTys             in
    let ctx'        =   add_retTy ctx rety              in
    let ctx''       =   add_local ctx' binds            in
    TmMthd(addTy_mthd_head cns head, addTy_stmts cns cn_name ctx'' body)

let unique_sig (TmCn(id,flds,mthds)) =
    let sigs        =   L.map (function | TmMthd(TyDefault,_)   -> None
                                        | TmMthd(tyM,_)         -> Some (str_of_tyMthd tyM)) mthds in
    let uniq_sigs   =   BL.unique sigs in
    L.length sigs=L.length uniq_sigs

let unique_name cns =
    let cnnames     = L.map(function _,TmCn(id,_,_) -> id) cns in
    L.length cns = L.length(BL.unique cnnames)

let addTy_cntrct cns (evs:ty idxlist) (TmCn(id,flds,mthds)) = 
    assert (BL.for_all(arg_has_known_ty (typeof_cns cns)) flds  && unique_sig (TmCn(id,flds,mthds)))  ; 
    let ctx         =   add_local (add_evnts empty_ctx(values evs)) (binds_of_tys flds) in
    TmCn(id,flds,L.map(addTy_mthd cns id ctx) mthds) 

let addTy_toplevel cns (evs:ty idxlist) = function 
    | TmEv e      -> TmEv e
    | t           -> addTy_cntrct cns evs t

let addTys (tops : unit toplevel idxlist)  =
    let cns         = filter_map (function  | TmEv e   -> None 
                                            | c        -> Some c ) tops in
    assert(unique_name cns);
    let evs         = filter_map (function  | TmEv e   -> Some e
                                            | _        -> None   ) tops in
    map (addTy_toplevel cns evs) tops
