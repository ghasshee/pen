open Big_int
open Printf
open Misc

module L    = List 
module BL   = BatList

type ty           (* atomic *)  =   TyVoid              (* 256 bits *) 
                  (* atomic *)  |   TyUint256           (* 256 bits *) 
                  (* atomic *)  |   TyUint8             (*   8 bits *) 
                  (* atomic *)  |   TyBytes32           (* 256 bits *) 
                  (* atomic *)  |   TyAddr              (* 160 bits *) 
                  (* atomic *)  |   TyBool 
                  (* atomic *)  |   TyRef               of ty 
                  (* atomic *)  |   TyTuple             of ty list
                  (* atomic *)  |   TyMap               of ty * ty 
                  (* atomic *)  |   TyInstnce           of string                       (* type of [b] declared as [bid b] *) 
                                |   TyMethod            of string * ty list * ty        (*  TyMethod(id, tyArgs, tyRet)             *)
                                |   TyDefault
                                |   TyAbs               of  ty * ty                     (*  TyAbs(tyArgs, tyRet)                    *)
                                |   TyVar               of string * ty                  (*  TyVar(id, ty)                           *) 
                                |   TyEvVar             of string * ty * bool           (*  TyEvVar(id,ty,indexed)                  *)
                                |   TyEvnt              of string * ty list 
                                |   TyCntrct            of string * ty list * ty list   (*  TyCntrct(id,tyCnArgs,tyMethod list *) 

let id_of_var (TyVar(id,_))     =   id 
let ty_of_var (TyVar(_,ty))     =   ty 

let rec string_of_ty            =   function 
    | TyUint256                 ->  "uint256"
    | TyUint8                   ->  "uint8" 
    | TyBytes32                 ->  "bytes32" 
    | TyAddr                    ->  "address" 
    | TyBool                    ->  "bool" 
    | TyRef              _      ->  "ref" 
    | TyTuple []                ->  "()"
    | TyTuple            _      ->  "tuple" 
    | TyMap(a,b)                ->  "mapping" 
    | TyCntrct(id,_,_)          ->  "contract arch "     ^ id
    | TyInstnce     s           ->  "contract instance " ^ s

let  arg_of_evnt_arg            =   function TyEvVar(id,ty,visible) -> TyVar(id,ty)
let args_of_evnt_args           =   L.map arg_of_evnt_arg

let split_evnt_args tyEv args   =   match tyEv with TyEvnt(id,tyEvArgs)  -> 
    let visibles : bool list    =   L.map (function TyEvVar(_,_,visible)->visible) tyEvArgs in
    let combined                =   L.combine args visibles in
    let is,ns                   =   BL.partition snd combined in
    L.map fst is, L.map fst ns

(*****************************************)
(***      STATEMENTS & EXPRESSIONS     ***)
(*****************************************)

type 'ty _call                  =   { call_id           : string
                                    ; call_args         : ('ty exprTy) list }
                                
and  'ty _new                   =   { new_id            : string
                                    ; new_args          : 'ty exprTy list
                                    ; new_msg           : 'ty exprTy        }
                                
and  'ty _send                  =   { sd_cn             : 'ty exprTy
                                    ; sd_mthd           : string option
                                    ; sd_args           : 'ty exprTy list
                                    ; sd_msg            : 'ty exprTy        }

and  'ty return                 =   { ret_expr          :  'ty exprTy 
                                    ; ret_cont          :  'ty exprTy       }
                                
and  'ty stmt                   =   
                                |   SmAssign            of 'ty lexpr * 'ty exprTy
                                |   SmDecl              of 'ty decl
                                |   SmIfThen            of 'ty exprTy * 'ty stmt list
                                |   SmIf                of 'ty exprTy * 'ty stmt list * 'ty stmt list
                                |   SmExpr              of 'ty exprTy
                                |   TmReturn            of 'ty exprTy * 'ty exprTy 

and  'ty exprTy                =   'ty expr * 'ty

and  'ty expr                   =   EpParen             of 'ty exprTy
                                |   TmId                of string
                                |   TmAbort 
                                |   TmDecl              of 'ty decl 
                                |   TmSlfDstrct         of 'ty exprTy
                                |   TmLog               of string * 'ty exprTy list * ty option (* ty := TyEvnt *)
                                |   TmRef               of 'ty exprTy
                                |   TmDeref             of 'ty exprTy
                                |   TmAssign            of 'ty lexpr * 'ty exprTy 
                                |   TmLoc               of int 
                                |   TmVar               of int * int 
                                |   TmAbs               of string * ty * 'ty exprTy
                                |   TmApp               of 'ty exprTy * 'ty exprTy  
                                |   TmIf                of 'ty exprTy * 'ty exprTy * 'ty exprTy 
                                |   TmFix               of 'ty exprTy
                                |   TmUnit 
                                |   EpTrue
                                |   EpFalse
                                |   EpUint256           of big_int
                                |   EpUint8             of big_int
                                |   EpNow
                                |   EpCall              of 'ty _call
                                |   EpNew               of 'ty _new
                                |   EpSend              of 'ty _send                    (* storage solidation *) 
                                |   EpLAnd              of 'ty exprTy * 'ty exprTy
                                |   EpLT                of 'ty exprTy * 'ty exprTy
                                |   EpGT                of 'ty exprTy * 'ty exprTy
                                |   EpEq                of 'ty exprTy * 'ty exprTy
                                |   EpNEq               of 'ty exprTy * 'ty exprTy
                                |   EpAddr              of 'ty exprTy
                                |   EpNot               of 'ty exprTy
                                |   EpArray             of 'ty array
                                |   EpValue
                                |   EpSender
                                |   EpThis
                                |   EpDeref             of 'ty exprTy
                                |   EpPlus              of 'ty exprTy * 'ty exprTy
                                |   EpMinus             of 'ty exprTy * 'ty exprTy
                                |   EpMult              of 'ty exprTy * 'ty exprTy
                                |   EpBalance           of 'ty exprTy

and 'ty lexpr                   =   LEpArray            of 'ty array

and 'ty array                   =   { arrId             :  'ty exprTy
                                    ; arrIndex          :  'ty exprTy         }

and 'ty decl                    =   { declTy            :   ty
                                    ; declId            :   string
                                    ; declVal           :  'ty exprTy         }


(*****************************************)
(***    METHODs      &    CONTRACTS    ***)
(*****************************************)

type 'ty mthd_body              =   'ty stmt list

type 'ty mthd                   =   { mthd_head         : ty
                                    ; mthd_body         : 'ty mthd_body }
                                
type 'ty cntrct                 =   { cn_id             : string
                                    ; fieldss           : ty list
                                    ; mthds             : 'ty mthd list }

(*****************************************)
(***           TOPLEVEL                ***)
(*****************************************)

type 'ty toplevel               =   Cntrct        of 'ty cntrct
                                |   Event         of ty    (* ty = TyEvnt *) 

let filter_usualMthd            =   BL.filter_map (function   | TyDefault                   -> None 
                                                              | TyMethod(i,a,r)             -> Some (TyMethod(i,a,r)) )  
let default_exists              =   L.exists      (function   | TyDefault                   -> true
                                                              | TyMethod(_,_,_)             -> false  )

let cntrct_name_of_ret_cont     = function 
    | EpCall c,_                -> Some c.call_id
    | _,_                       -> None

let argTys_of_mthd                = function 
    | TyMethod(_,argTys,_)      -> argTys
    | TyDefault                 -> []

let cntrct_name_of_instance     = function
    | _,(TyInstnce cn,_)        -> cn


(*****************************************)
(***           PRINTING                ***)
(*****************************************)

let string_of_expr              = function 
    | TmAbort                   -> "abort" 
    (*| TmReturn(_,_)             -> "return"*)
    | TmLog(_,_,_)              -> "log"
    | TmSlfDstrct _             -> "selfdestruct"
    | TmId        str           -> "id " ^ str
    | EpThis                    -> "this"
    | EpArray       _           -> "a[idx]"
    | EpSend        _           -> "send"
    | EpNew         _           -> "new"
    | EpParen       _           -> "()"
    | EpCall      _             -> "call"
    | EpNow                     -> "now"
    | EpSender                  -> "sender"
    | EpTrue                    -> "true"
    | EpFalse                   -> "false"
    | EpUint256   d             -> "declit " ^ string_of_big d
    | EpUint8     d             -> "declit " ^ string_of_big d
    | EpNot         _           -> "not"
    | EpNEq         _           -> "neq"
    | EpLAnd        _           -> "_ && _"
    | EpLT          _           -> "lt"
    | EpGT          _           -> "gt"
    | EpValue                   -> "value"
    | EpEq          _           -> "equality"
    | EpAddr        _           -> "address"
    | EpDeref _                 -> "dereference of ..."
    | EpPlus     (a,b)          -> "... + ..."
    | EpMinus    (a,b)          -> "... - ..."
    | EpMult     (a,b)          -> "... * ..."
    | EpBalance     _           -> "balance"

(*****************************************)
(***              SIZE                 ***)
(*****************************************)


let size_of_ty (* in bytes *)   = function
    | TyUint8                   ->  1
    | TyUint256                 -> 32
    | TyBytes32                 -> 32
    | TyAddr                    -> 20
    | TyInstnce _               -> 20 (* address as word *)
    | TyBool                    -> 32
    | TyRef _                   -> 32
    | TyTuple []                -> err "size_of_ty TyUnit" 
    | TyTuple _                 -> err "size_of_ty TyTuple"
    | TyMap   _                 -> err "size_of_ty TyMap" 
    | TyCntrct(id,_,_)          -> err("size_of_ty TyCntrct: " ^ id)

let size_of_tys                 = BL.sum $ (L.map size_of_ty) 

let calldata_size_of_ty         = function 
    | TyMap _                   -> err "mapping cannot be a method arg"
    | TyRef _                   -> err "reference type cannot be a method arg"
    | TyTuple _                 -> err "tupletype not implemented"
    | TyCntrct _                -> err "cntrct type cannot be a method arg"
    | tyT                       -> size_of_ty tyT

let calldata_size_of_arg (TyVar(_,ty))    = calldata_size_of_ty ty

(*****************************************)
(***            TYPE auxirity          ***)
(*****************************************)

let is_mapping                  = function 
    | TyMap         _           -> true
    | _                         -> false 

let non_mapping_arg             = function 
    | TyVar(_,TyMap _)          -> false
    | TyVar(_,_)                -> true 
    | _                         -> err "not an arg"

let count_plain_args            = L.length $ (L.filter (not $ is_mapping)) 





(* -------------------------------------------------- *) 
(* Shifting *)
(*
let rec tyWalk onVar c          = let f = onVar in function 
    | TyVariant(fieldtys)       -> TyVariant(List.map (fun(l,tyT)->(l,tyWalk f c tyT)) fieldtys) 
    | TyRecord(fieldtys)        -> TyRecord(List.map (fun(l,tyT)->(l,tyWalk f c tyT)) fieldtys) 
    | TyArr(tyT1,tyT2)          -> TyArr(tyWalk f c tyT1,tyWalk f c tyT2) 
    | TyVar(x,n)                -> onVar c x n
    | TyRef(tyT)                -> TyRef(tyWalk f c tyT) 
    | tyT                       -> tyT

and  'ty expr                   =   EpParen             of 'ty exprTy
                                (* Ref *)
                                |   TmDecl              of 'ty decl 
                                |   TmSlfDstrct         of 'ty exprTy
                                |   TmLog               of string * 'ty exprTy list * tyEvnt option 
                                |   TmRef               of 'ty exprTy
                                |   TmDeref             of 'ty exprTy
                                |   TmAssign            of 'ty lexpr * 'ty exprTy 
                                |   TmLoc               of int 
                                |   TmVar               of int * int 
                                |   TmAbs               of string * ty * 'ty exprTy
                                |   TmApp               of 'ty exprTy * 'ty exprTy  
                                |   TmIf                of 'ty exprTy * 'ty exprTy * 'ty exprTy 
                                |   TmFix               of 'ty exprTy
                                |   EpTrue
                                |   EpFalse
                                |   EpUint256           of big_int
                                |   EpUint8             of big_int
                                |   EpNow
                                |   TmId             of string
                                |   EpCall              of 'ty _call
                                |   EpNew               of 'ty _new
                                |   EpSend              of 'ty _send
                                |   EpLAnd              of 'ty exprTy * 'ty exprTy
                                |   EpLT                of 'ty exprTy * 'ty exprTy
                                |   EpGT                of 'ty exprTy * 'ty exprTy
                                |   EpEq                of 'ty exprTy * 'ty exprTy
                                |   EpNEq               of 'ty exprTy * 'ty exprTy
                                |   EpAddr              of 'ty exprTy
                                |   EpNot               of 'ty exprTy
                                |   EpArray             of 'ty array
                                |   EpValue
                                |   EpSender
                                |   EpThis
                                |   EpDeref             of 'ty exprTy
                                |   EpPlus              of 'ty exprTy * 'ty exprTy
                                |   EpMinus             of 'ty exprTy * 'ty exprTy
                                |   EpMult              of 'ty exprTy * 'ty exprTy
                                |   EpBalance           of 'ty exprTy

let rec tmWalk onVar onType c   = let (f,g) = (onVar,onType) in function 
    | TmSucc(t)                 -> TmSucc(tmWalk f g c t) 
    | TmRef(t)                  -> TmRef(tmWalk f g c t)
    | TmDeref(t)                -> TmDeref(tmWalk f g c t) 
    | TmAssign(t1,t2)           -> TmAssign(tmWalk f g c t1,tmWalk f g c t2)
    | TmCase(t,cases)           -> TmCase(tmWalk f g c t,L.map(fun(li,(xi,ti))->li,(xi,tmWalk f g(c+1)ti))cases)
    | TmLet(x,t1,t2)            -> TmLet(x,tmWalk f g c t1, tmWalk f g(c+1)t2) 
    | TmAbs(x,tyT,t2)           -> TmAbs(x,g c tyT,tmWalk f g(c+1)t2)
    | TmApp(t1,t2)              -> TmApp(tmWalk f g c t1, tmWalk f g c t2) 
    | TmIf(t1,t2,t3)            -> TmIf(tmWalk f g c t1, tmWalk f g c t2, tmWalk f g c t3) 
    | TmPred(t)                 -> TmPred(tmWalk f g c t) 
    | TmIsZero(t)               -> TmIsZero(tmWalk f g c t)
    | TmAscribe(t,tyT)          -> TmAscribe(tmWalk f g c t,g c tyT) 
    | TmRecord(tl)              -> TmRecord(L.map(fun(l,t)->(l,tmWalk f g c t))tl)  
    | TmProj(t,i)               -> TmProj(tmWalk f g c t,i) 
    | TmFix(t)                  -> TmFix(tmWalk f g c t)
    | TmVar(x,n)                -> onVar c x n
    | x                         -> x

let tyShiftOnVar d          = fun c x n     ->  if x>=c then (TyVar(x+d,n+d) :ty')    else (TyVar(x,n+d) : ty')
let tyShiftAbove d          = tyWalk (tyShiftOnVar d) 
let tyShift d               = (*if d>=0 then pe("TYVARSHIFT    : "^(soi d));*)tyShiftAbove d 0    

let tmShiftOnVar d          = fun c x n  ->  if x>=c then (TmVar(x+d,n+d) : term)  else TmVar(x,n+d)
let tmShiftAbove d          = tmWalk (tmShiftOnVar d) (tyShiftAbove d) 
let tmShift d               = (*if d>=0 then pe("TMVARSHIFT    : "^(soi d));*)tmShiftAbove d 0 

(*
let bindshift d             = function 
    | BindTyAbb(tyT)            ->  BindTyAbb(tyShift d tyT) 
    | BindTmVar(tyT)            ->  BindTmVar(tyShift d tyT) 
    | BindTmAbb(t,tyT_opt)      ->  (let tyT_opt' = match tyT_opt with 
                                        | None      -> None
                                        | Some(tyT) -> Some(tyShift d tyT) in 
                                    BindTmAbb(tmShift d t, tyT_opt'))
    | b                         ->  b
*)
                                        
(* -------------------------------------------------- *) 
(* Substitution *) 
let tySubstOnVar j tyS tyT  = fun    c x n ->   if x=j+c then tyShift c tyS else (TyVar(x,n) : ty') 
let tySubst      j tyS tyT  = tyWalk(tySubstOnVar j tyS tyT)0 tyT

let tmSubstOnVar j s t      = fun c x n ->   if x=j+c then tmShift c s else TmVar( x, n) 
let tmSubst      j s t      = tmWalk (tmSubstOnVar j s t) (fun k x -> x) 0 t
let tmSubstTop     s t      = tmShift (-1) (tmSubst 0 (tmShift 1 s) t) 

*)
