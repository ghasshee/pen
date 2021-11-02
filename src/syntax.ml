open Printf
open Misc
open Semiring 

module L    = List 
module BL   = BatList




(**********************************)
(***          Types             ***)
(**********************************)

type ty           (* atomic *)  =   TyErr                             
                  (* stomic *)  |   TyUnit             
                  (* atomic *)  |   TyU256              (* 256 bits *) 
                  (* atomic *)  |   TyU8                (*   8 bits *) 
                  (* atomic *)  |   TyBytes32           (* 256 bits *) 
                  (* atomic *)  |   TyAddr              (* 160 bits *) 
                  (* atomic *)  |   TyBool 
                  (* atomic *)  |   TyTuple             of ty list
                  (* atomic *)  |   TyMap               of ty * ty 
                  (* atomic *)  |   TyInstnc            of str                       
 (* TyMthd(id,args,ret)     *)  |   TyMthd              of str * ty list * ty        
                                |   TyDefault
                                |   TyRef               of ty 
 (* TyAbs(arg, ret)         *)  |   TyAbs               of  ty * ty                  
                                |   TyIdx               of int * int                 
 (* TyVar(id, ty)           *)  |   TyVar               of str * ty                  
 (* TyEvVar(id,ty,indexed)  *)  |   TyEvVar             of str * ty * bool           
 (* TyEv(  ,   )            *)  |   TyEv                of str * ty list 
 (* TyCn(id,fields,mthds)   *)  |   TyCn                of str * ty list * ty list   


let id_of_var                   =   function TyVar(id,_) -> id          | _ -> err "id_of_var" 
let ty_of_var                   =   function TyVar(_,ty) -> ty          | _ -> err "id_of_var" 
let tys_of_vars                 =   L.map ty_of_var 

let rec str_of_ty            =   function 
    | TyU256                    ->  "uint256"
    | TyU8                      ->  "uint8" 
    | TyBytes32                 ->  "bytes32" 
    | TyAddr                    ->  "address" 
    | TyBool                    ->  "bool" 
    | TyRef              _      ->  "ref" 
    | TyUnit                    ->  "()"
    | TyTuple            _      ->  "tuple" 
    | TyMap(a,b)                ->  "mapping" 
    | TyCn(id,_,_)              ->  "contract arch "     ^ id
    | TyInstnc     s            ->  "contract instance " ^ s
    | TyMthd(id,_,_)            ->  "method " ^ id 
    | TyDefault                 ->  "default" 
    | TyAbs(a,b)                ->  str_of_ty a ^ "→" ^ str_of_ty b
    | _                         ->  "undefined" 

let  arg_of_ev_arg              =   function TyEvVar(id,ty,_) -> TyVar(id,ty)
let args_of_ev_args             =   L.map arg_of_ev_arg

let split_ev_args tyEv args     =   match tyEv with TyEv(id,evargs)  -> 
    let visibles : bool list        =   L.map (function TyEvVar(_,_,visible)->visible) evargs in
    let combined                    =   zip args visibles                   in
    let is,ns                       =   BL.partition snd combined           in
    L.map fst is, L.map fst ns


    
(**********************************)
(***          Terms             ***)
(**********************************)

type 'ty toplevel               =   TmCn        of str * ty list * 'ty mthd list  (* TmCn(id,fields,mthds *) 
                                |   TmEv        of ty                                (* TmEv(tyEv)           *) 

and  'ty mthd                   =   TmMthd      of ty * 'ty stmt list 

and  'ty stmt                   =   SmExpr      of 'ty exprTy
                               (*  |   SmDecl      of ty * str * 'ty exprTy *) 
                                |   SmIf        of 'ty exprTy * 'ty stmt list * 'ty stmt list

and  'ty exprTy                 =   'ty expr * 'ty

and  'ty expr                   =   
                                |   SmAssign    of 'ty expr * 'ty exprTy
                                |   TmRef       of 'ty exprTy
                                |   TmDeref     of 'ty exprTy
                                |   TmAssign    of 'ty expr * 'ty exprTy 
                                |   TmLoc       of int 
                                |   EpDeref     of 'ty exprTy
                                |   TmApp       of 'ty exprTy * 'ty exprTy  
                                |   TmAbs       of str * ty * 'ty exprTy
                                |   TmFix       of str * str * ty * 'ty exprTy
                                |   TmIdx       of int * int 
                                |   TmIdxRec    of int 
                                |   TmIdxStrct  of int 
                                |   TmId        of str
                                |   TmIf        of 'ty exprTy * 'ty exprTy * 'ty exprTy 
(* TmReturn(ret,cont)        *) |   TmReturn    of 'ty exprTy * 'ty exprTy  
(* TmCall(cnname, args)      *) |   TmCall      of str * 'ty exprTy list       
(* TmArray(id,idx)           *) |   TmArray     of 'ty exprTy * 'ty exprTy      
(* TmSend(cn,mname,args,msg) *) |   TmSend      of 'ty exprTy * str option * 'ty exprTy list * 'ty exprTy
(* TmNew(id,args,msg)        *) |   TmNew       of str * 'ty exprTy list * 'ty exprTy    
                                |   TmSlfDstrct of 'ty exprTy
                                |   TmLog       of str * 'ty exprTy list * ty option (* ty := TyEv *)
                                |   TmAbort 
                                |   TmUnit 
                                |   TmZero 
                                |   TmTrue
                                |   TmFalse
                                |   TmU256      of big
                                |   TmU8        of big
                                |   TmAdd       of 'ty exprTy * 'ty exprTy
                                |   TmSub       of 'ty exprTy * 'ty exprTy
                                |   TmMul       of 'ty exprTy * 'ty exprTy
                                |   EpNow
                                |   EpLAnd      of 'ty exprTy * 'ty exprTy
                                |   EpLT        of 'ty exprTy * 'ty exprTy
                                |   EpGT        of 'ty exprTy * 'ty exprTy
                                |   TmEq        of 'ty exprTy * 'ty exprTy
                                |   EpNEq       of 'ty exprTy * 'ty exprTy
                                |   EpAddr      of 'ty exprTy
                                |   EpNot       of 'ty exprTy
                                |   EpValue
                                |   EpSender
                                |   EpThis
                                |   Balanc      of 'ty exprTy

let get_ty  (_,ty)              =   ty
let get_tm  (x,_)               =   x


(*****************************************)
(***    METHODs      &    CONTRACTS    ***)
(*****************************************)

let filter_vars                 =   L.filter (function TyVar(_,TyMap _) -> false | TyVar _ -> true )
let filter_arrs                 =   L.filter (function TyVar(_,TyMap _) -> true  | TyVar _ -> false) 
let varTys_of_cn(TmCn(_,flds,_))=   filter_vars flds
let arrTys_of_cn(TmCn(_,flds,_))=   filter_arrs flds
let fldTys_of_cn(TmCn(_,flds,_))=   tys_of_vars flds

let str_of_tyMthd (TyMthd(id,args,ret))  =
    let argTys          = tys_of_vars (filter_vars args)        in
    let strTys          = L.map str_of_ty argTys                in
    let tys             = S.concat "," strTys                   in
    id    ^ "(" ^ tys ^ ")"

let str_of_evnt   (TyEv(id,tyEvArgs))    = 
    let args            = args_of_ev_args tyEvArgs              in 
    let argTys          = tys_of_vars (filter_vars args)        in
    let strTys          = L.map str_of_ty argTys                in
    let tys             = S.concat "," strTys                   in
    id ^ "(" ^ tys ^ ")"

(*****************************************)
(***           TOPLEVEL                ***)
(*****************************************)

let filter_method               =   BL.filter_map (function   | TyDefault             -> None 
                                                              | TyMthd(i,a,r)         -> Some (TyMthd(i,a,r)) )  
let default_exists              =   L.exists      (function   | TyDefault             -> true
                                                              | TyMthd(_,_,_)         -> false  )

let cnname_of_ret_cont          = function 
    | TmCall(id,args),_         -> Some id
    | _,_                       -> None

let argTys_of_mthd              = function 
    | TyMthd(_,argTys,_)        -> argTys
    | TyDefault                 -> []


(*****************************************)
(***           PRINTING                ***)
(*****************************************)


let rec str_of_expr          = function 
    | EpThis                    -> "this"
    | EpNow                     -> "now"
    | EpSender                  -> "sender"
    | EpValue                   -> "value"
    | EpNot         _           -> "not"
    | EpNEq         _           -> "neq"
    | EpLAnd        _           -> "_ && _"
    | EpLT          _           -> "lt"
    | EpGT          _           -> "gt"
    | EpAddr        _           -> "address"
    | EpDeref       _           -> "dereference of ..."
    | Balanc        _           -> "balance" 


let rec str_of_tm  e         = match fst e with 
    | TmApp(t1,t2)              -> "TmApp(" ^ str_of_tm t1 ^ "," ^ str_of_tm t2 ^ ")"
    | TmAbs(x,tyX,t)            -> "(λ" ^ x ^ ":" ^ str_of_ty tyX ^ "." ^ str_of_tm t ^ ")"
    | TmFix(f,n,_,t)            -> "TmFix(λ" ^ f ^ " " ^ n ^ "→" ^ str_of_tm t ^ ")" 
    | TmId(s)                   -> "TmId(" ^ s ^ ")"
    | TmIdx(i,n)                -> "TmIdx"      ^ str_of_int i 
    | TmIdxRec(i)               -> "TmRec"      ^ str_of_int i 
    | TmIdxStrct(i)             -> "{Struct "   ^ str_of_int i ^ "}"
    | TmIf(b,t,t')              -> "(If " ^ str_of_tm b ^ " Then " ^ str_of_tm t ^ " Else " ^ str_of_tm t' ^ ")"
    | TmEq(a,b)                 -> str_of_tm a ^ "==" ^ str_of_tm b
    | TmU256(b)                 -> "u256" ^ str_of_big b 
    | TmU8(b)                   -> "u8"   ^ str_of_big b 
    | TmId(str)                 -> str
    | TmMul(a,b)                -> str_of_tm a ^ " * " ^ str_of_tm b
    | TmSub(a,b)                -> str_of_tm a ^ " - " ^ str_of_tm b 
    | TmAdd(a,b)                -> str_of_tm a ^ " + " ^ str_of_tm b
    | TmUnit                    -> "()" 
    | TmZero                    -> "O" 
    | TmTrue                    -> "true"
    | TmFalse                   -> "false"
    | TmSend _                  -> "TmSend()" 
    | TmArray(id,idx)           -> str_of_tm id ^ "[" ^ str_of_tm idx ^ "]" 
    | TmCall(id,args)           -> "TmCall(" ^ id ^ ")" 
    | TmAbort                   -> "abort" 
    | TmReturn(r,_)             -> "return " ^ str_of_tm r 
    | TmLog(_,_,_)              -> "log"
    | TmNew         _           -> "new"
    | TmSlfDstrct   _           -> "selfdestruct"
    | SmAssign(l,r)             -> "assignment" 
    | e                         ->  str_of_expr e 


(*****************************************)
(***              SIZE                 ***)
(*****************************************)

let size_of_ty (* in bytes *)   = function
    | TyU8                      ->  1
    | TyU256                    -> 32
    | TyBytes32                 -> 32
    | TyAddr                    -> 20
    | TyInstnc _                -> 20 (* address as word *)
    | TyBool                    -> 32
    | TyRef     _               -> 32
    | TyUnit                    -> err "size_of_ty TyUnit" 
    | TyTuple   _               -> err "size_of_ty TyTuple"
    | TyMap     _               -> err "size_of_ty TyMap" 
    | TyCn(id,_,_)              -> err("size_of_ty TyCn: " ^ id)

let size_of_tys                 = BL.sum $ L.map size_of_ty 
let size_of_args                = BL.sum $ L.map (size_of_ty $ ty_of_var) 
let size_of_vars_in_cn cn       = ($) size_of_tys (L.map ty_of_var $ varTys_of_cn) cn

let calldata_size_of_ty         = function 
    | TyMap _                   -> err "mapping cannot be a method arg"
    | TyRef _                   -> err "reference type cannot be a method arg"
    | TyTuple _                 -> err "tupletype not implemented"
    | TyCn _                    -> err "cntrct type cannot be a method arg"
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

let rec tyWalk onVar c          = let f = onVar in function 
    | TyAbs(tyT1,tyT2)          -> TyAbs(tyWalk f c tyT1,tyWalk f c tyT2) 
    | TyIdx(x,n)                -> onVar c x n
    | tyT                       -> tyT

let rec tmWalk onVar onType c   = let (f,g) = (onVar,onType) in function 
   (* | TmLet(x,t1,t2)            -> TmLet(x,tmWalk f g c t1, tmWalk f g(c+1)t2) *)
    | TmAbs(x,tyT,(t2,b))       -> TmAbs(x,g c tyT,(tmWalk f g(c+1)t2,b))
    | TmApp((t1,a),(t2,b))      -> TmApp((tmWalk f g c t1,a),(tmWalk f g c t2,b)) 
    | TmIdx(x,n)                -> onVar c x n
    | x                         -> x

let tyShiftOnVar d          = fun c x n  ->  if x>=c then TyIdx(x+d,n+d) else TyIdx(x,n+d)
let tyShiftAbove d          = tyWalk (tyShiftOnVar d) 
let tyShift d               = tyShiftAbove d 0    

let tmShiftOnVar d          = fun c x n  ->  if x>=c then TmIdx(x+d,n+d) else TmIdx(x,n+d)
let tmShiftAbove d          = tmWalk (tmShiftOnVar d) (tyShiftAbove d) 
let tmShift d               = tmShiftAbove d 0 

(*
let bindshift d             = function 
    | BindTyAbb(tyT)            ->  BindTyAbb(tyShift d tyT) 
    | BindTmIdx(tyT)            ->  BindTmIdx(tyShift d tyT) 
    | BindTmAbb(t,tyT_opt)      ->  (let tyT_opt' = match tyT_opt with 
                                        | None      -> None
                                        | Some(tyT) -> Some(tyShift d tyT) in 
                                    BindTmAbb(tmShift d t, tyT_opt'))
    | b                         ->  b
*)                                      
(* -------------------------------------------------- *) 
(* Substitution *) 
let tySubstOnVar j tyS tyT  = fun c x n -> if x=j+c then tyShift c tyS else TyIdx(x,n)
let tySubst      j tyS tyT  = tyWalk(tySubstOnVar j tyS tyT)0 tyT

let tmSubstOnVar j s t      = fun c x n -> if x=j+c then tmShift c s   else TmIdx( x, n) 
let tmSubst      j s t      = tmWalk (tmSubstOnVar j s t) (fun k x -> x) 0 t
let tmSubstTop     s t      = tmShift (-1) (tmSubst 0 (tmShift 1 s) t) 

