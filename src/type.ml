open Printf 
open Misc 
open Syntax
open IndexList
open TypeEnv

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

let typeof_cntrct(cn:'a cntrct) =   { id                = cn.cntrct_id
                                    ; tyCnArgs          = L.map ty_of_var cn.cntrct_args
                                    ; tyCnMthds         = L.map typeof_mthd cn.mthds            }

let id_lookup_ty ctx id         =   match lookup_id id ctx with
    | Some tyT                      ->  EpIdent id, tyT
    | None                          ->  err ("unknown identifier "^id)

let tycn_has_name  name  tycn   =             tycn.id = name 
let itycn_has_name name itycn   =   (get_ty itycn).id = name 
let is_known_cntrct tycns nm    =   BL.exists (itycn_has_name nm) tycns

let rec is_known_ty cns         =   function 
    | TyUint256 | TyUint8           ->  true
    | TyBytes32 | TyAddr            ->  true
    | TyBool                        ->  true
    | TyTuple []                    ->  true
    | TyTuple l                     ->  BL.for_all (is_known_ty cns) l
    | TyRef l                       ->  is_known_ty cns l
    | TyMap(a,b)                    ->  is_known_ty cns a && is_known_ty cns b
    | TyInstnce cn                  ->  is_known_cntrct cns cn

let arg_has_known_ty cns        =   function 
    | TyVar(id,ty)                  ->  let ret = is_known_ty cns ty in
                                        if not ret 
                                            then err("Unknown Type Arg "^string_of_ty ty)
                                            else ret

let addTy_mthd_head cns         =   function 
    | TyDefault                     ->  TyDefault
    | TyMethod(id,argTys,retTy)     ->  assert (BL.for_all (arg_has_known_ty cns) argTys) ; 
                                        assert (is_known_ty cns retTy) ;
                                        TyMethod(id,argTys,retTy)

let call_arg_expectations cns   =   function 
    | "pre_ecdsarecover"            ->  (=) [TyBytes32;TyUint8;TyBytes32;TyBytes32]
    | "keccak256"                   ->  konst true
    | "iszero"                      ->  fun x -> x=[TyBytes32]||x=[TyUint8]||x=[TyUint256]||x=[TyBool]||x=[TyAddr]
    | name                          ->  let cnIdx   = lookup_idx (tycn_has_name name) cns in
                                        let tyCn    = lookup_index cnIdx cns in
                                        (=) tyCn.tyCnArgs

let typecheck  (ty,(_,t))       =   assert (ty = t)
let typechecks tys actual       =   L.for_all2 (fun ty (_,a)-> ty=a) tys actual

let check_args_match cns args   =   function 
    | Some m                        ->  assert (call_arg_expectations cns m (L.map get_ty args))
    | None                          ->  assert (isNil (L.map get_ty args))

let rec addTy_call cns cname ctx c =
    let args   = L.map (addTy_expr cns cname ctx) c.call_args in
    check_args_match cns args (Some c.call_id) ; 
    let reT    = match c.call_id with
        | "value" when true         ->  TyUint256 (* check the arg is 'msg' *) 
        | "pre_ecdsarecover"        ->  TyAddr
        | "keccak256"               ->  TyBytes32
        | "iszero"                  ->  begin match args with
            | [arg]                     ->  TyBool
            | _                         ->  err "should not happen" end 
        | cnname when true          ->  TyCntrct cnname (* check contract exists *) 
        | _                         ->  err "addTy_call: should not happen" in
    { call_id   = c.call_id 
    ; call_args = args      }, reT


and addTy_expr cns cname ctx (expr,()) =    match expr with
 (* | SmAbort                       ->  SmAbort         , TyVoid *)
    | EpParen     e                 ->  addTy_expr cns cname ctx e 
    | EpThis                        ->  EpThis          , TyInstnce cname
    | EpTrue                        ->  EpTrue          , TyBool
    | EpFalse                       ->  EpFalse         , TyBool
    | EpSender                      ->  EpSender        , TyAddr
    | EpNow                         ->  EpNow           , TyUint256
    | EpDecLit256 d                 ->  EpDecLit256 d   , TyUint256
    | EpDecLit8   d                 ->  EpDecLit8   d   , TyUint8
    | EpValue                       ->  EpValue         , TyUint256
    | EpAddr      e                 ->  let e       = addTy_expr cns cname ctx e          in
                                        EpAddr e        , TyAddr
    | EpCall    c                 ->  let c,ty    = addTy_call cns cname ctx c          in
                                        EpCall c      , ty
    | EpIdent     s                 ->  check_reserved s ; 
                                        id_lookup_ty ctx s
    | EpBalance   e                 ->  let e       = addTy_expr cns cname ctx e          in
                                        assert (acceptable_as TyAddr (get_ty e));
                                        EpBalance e     , TyUint256
    | EpNew       n                 ->  let n,nm    = addTy_new cns cname ctx n           in
                                        check_reserved nm;  
                                        EpNew n         , TyInstnce nm
    | EpLAnd (l, r)                 ->  let l       = addTy_expr cns cname ctx l          in
                                        typecheck (TyBool,l);
                                        let r       = addTy_expr cns cname ctx r          in
                                        typecheck (TyBool,r); 
                                        EpLAnd(l,r)     , TyBool
    | EpLT (l, r)                   ->  let l       = addTy_expr cns cname ctx l          in
                                        let r       = addTy_expr cns cname ctx r          in
                                        assert_tyeqv l r ; 
                                        EpLT(l,r)       , TyBool
    | EpGT (l, r)                   ->  let l       = addTy_expr cns cname ctx l          in
                                        let r       = addTy_expr cns cname ctx r          in
                                        assert_tyeqv l r ; 
                                        EpGT (l, r)     , TyBool
    | EpNEq (l, r)                  ->  let l       = addTy_expr cns cname ctx l          in
                                        let r       = addTy_expr cns cname ctx r          in
                                        assert_tyeqv l r ;
                                        EpNEq (l, r)    , TyBool
    | EpEq (l, r)                   ->  let l       = addTy_expr cns cname ctx l          in
                                        let r       = addTy_expr cns cname ctx r          in
                                        assert_tyeqv l r ; 
                                        EpEq (l, r)     , TyBool
    | EpPlus (l, r)                 ->  let l       = addTy_expr cns cname ctx l          in
                                        let r       = addTy_expr cns cname ctx r          in
                                        assert_tyeqv l r ;
                                        EpPlus (l, r)   , get_ty l
    | EpMinus (l, r)                ->  let l       = addTy_expr cns cname ctx l          in
                                        let r       = addTy_expr cns cname ctx r          in
                                        assert_tyeqv l r; 
                                        EpMinus (l, r)  , get_ty l
    | EpMult (l, r)                 ->  let l       = addTy_expr cns cname ctx l          in
                                        let r       = addTy_expr cns cname ctx r          in
                                        assert_tyeqv l r; 
                                        EpMult (l, r)   , snd l
    | EpNot  e                      ->  let e       = addTy_expr cns cname ctx e          in
                                        assert (get_ty e=TyBool) ; 
                                        EpNot e         , TyBool
    | EpArray a                     ->  let e       = addTy_expr cns cname ctx a.arrId    in
                                        begin match get_ty e with | TyMap(kT,vT)  ->  
                                        let idx,ty  = addTy_expr cns cname ctx a.arrIndex in 
                                        assert (acceptable_as kT ty) ; 
                                        EpArray { arrId    = e
                                                ; arrIndex = idx,ty }, vT end  
    | EpSend sd                     ->  let msg     = addTy_expr  cns cname ctx sd.sd_msg  in
                                        let cn      = addTy_expr cns cname ctx sd.sd_cn   in
                                        begin match sd.sd_mthd with
                                        | Some m ->  
                                        let TyMethod(_,_,tyRet)  =   find_tyMthd cns m                 in            
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
    let retTyexpr   =   BO.map (addTy_expr cns cname ctx) ret.ret_expr in
    let retTyCheck  =   lookup_retTyCheck ctx in
    assert (retTyCheck(BO.map get_ty retTyexpr)); 
    { ret_expr      =   retTyexpr
    ; ret_cont      =   addTy_expr cns cname ctx ret.ret_cont }

and addTy_decl cns cname ctx vd =
    let v           =   addTy_expr cns cname ctx vd.declVal in
    let id          =   vd.declId     in
    let ty          =   vd.declTy     in
    check_reserved id ; assert (is_known_ty cns ty);
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


(* evaluation *) 
let eval_expr ctx e = e  (* #TODO *) 

(* Termination *) 


type termination            =
                            | OnTheWay 
                            | ReturnBySize of int 
                            | JustStop



let rec is_terminating      = function 
                                    | SmAbort   -> [JustStop] 
    | SmSlfDstrct _             -> [JustStop]
    | SmAssign    _             -> [OnTheWay]
    | SmDecl      _             -> [OnTheWay]
    | SmExpr  (e,_)             -> (match eval_expr empty_ctx e with 
                                    | _         -> [OnTheWay] )
    | SmLog       _             -> [OnTheWay]
   (* | SmIfThen(_,b)             -> are_terminating b  @ [OnTheWay] (* there is a continuation if the condition does not hold. *)
    | SmIf(_,bT,bF)             -> are_terminating bT @ (are_terminating bF) *)
    | SmReturn ret              -> begin match ret.ret_expr with
        | Some _                    -> [ReturnBySize 1]
        | None                      -> [ReturnBySize 0] end

and are_terminating stmts =
  let last_stmt = BL.last stmts in
  is_terminating last_stmt



(* AssignTy Method / Contract / Toplevel  *) 
(* Default Method Returns Unit(==EmptyTuple) is a specification *) 

let mthd_returns_unit (mthd:unit mthd) = match mthd.mthd_head with
    | TyDefault             ->  true
    | TyMethod(_,_,retTy)   ->  retTy = TyTuple[]

let retTyCheck_of_mthd m ty_inferred = match m, ty_inferred with
    | TyDefault ,Some _       ->  false
    | TyDefault ,None         ->  true
    | TyMethod(_,_,retTy), _  ->  begin match retTy, ty_inferred with
        | _, None               -> true
        | x, Some y             -> acceptable_as x y
        | _, _                  -> false  end

let addTy_mthd cns cn_name ctx (m:unit mthd) =
    assert (L.for_all (function 
                         | OnTheWay         -> false
                         | ReturnBySize 0   -> mthd_returns_unit m
                         | ReturnBySize 1   -> not (mthd_returns_unit m)
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
                              | TyDefault   -> None
                              | tyM         -> Some (string_of_tyMthd tyM) ) mthds in
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

let has_distinct_cntrct_names (cns : unit cntrct idx_list) : bool =
    let cn_names    = (L.map(fun(_,b)->b.cntrct_id)cns) in
    L.length cns=L.length(BL.unique cn_names)

let addTys (tops : unit toplevel idx_list) : ty toplevel idx_list =
    let cntrcts                 = filter_map (function  | Cntrct c -> Some c
                                                        | _        -> None   ) tops in
    assert(has_distinct_cntrct_names(cntrcts));
    let tys                     = map typeof_cntrct cntrcts in
    let evs : tyEvnt idx_list   = filter_map (function  | Event e  -> Some e
                                                        | _        -> None   ) tops in
    map (addTy_toplevel tys evs) tops
