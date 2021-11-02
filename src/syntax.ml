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
                                |   TyI               of int * int                 
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

type 'ty toplevel               =   TmCn        of str * ty list * 'ty mthd list    (* TmCn(id,fields,mthds *) 
                                |   TmEv        of ty                               (* TmEv(tyEv)           *) 

and  'ty mthd                   =   TmMthd      of ty * 'ty tmty

and  'ty tmty                 =   'ty tm * 'ty

and  'ty tm                   =   
                                |   TmRef       of 'ty tmty
                                |   TmDeref     of 'ty tmty
                                |   TmAssign    of 'ty tmty * 'ty tmty 
                                |   TmLoc       of int 
                                |   TmApp       of 'ty tmty * 'ty tmty  
                                |   TmAbs       of str * ty * 'ty tmty
                                |   TmFix       of str * str * ty * 'ty tmty
                                |   TmI         of int * int 
                                |   TmIRec      of int 
                                |   TmIStrct    of int 
                                |   TmId        of str
                                |   TmIf        of 'ty tmty * 'ty tmty * 'ty tmty 
(* TmReturn(ret,cont)        *) |   TmReturn    of 'ty tmty * 'ty tmty  
(* TmCall(cnname, args)      *) |   TmCall      of str * 'ty tmty list       
(* TmArray(id,idx)           *) |   TmArray     of 'ty tmty * 'ty tmty      
(* TmSend(cn,mname,args,msg) *) |   TmSend      of 'ty tmty * str option * 'ty tmty list * 'ty tmty
(* TmNew(id,args,msg)        *) |   TmNew       of str * 'ty tmty list * 'ty tmty    
                                |   TmSfDstr    of 'ty tmty
                                |   TmLog       of str * 'ty tmty list * ty option (* ty := TyEv *)
                                |   TmAbort 
                                |   TmUnit 
                                |   TmZero 
                                |   TmTrue
                                |   TmFalse
                                |   TmU256      of big
                                |   TmU8        of big
                                |   TmAdd       of 'ty tmty * 'ty tmty
                                |   TmSub       of 'ty tmty * 'ty tmty
                                |   TmMul       of 'ty tmty * 'ty tmty
                                |   EpNow
                                |   TmLAND      of 'ty tmty * 'ty tmty
                                |   TmLT        of 'ty tmty * 'ty tmty
                                |   TmGT        of 'ty tmty * 'ty tmty
                                |   TmEQ        of 'ty tmty * 'ty tmty
                                |   TmNEQ       of 'ty tmty * 'ty tmty
                                |   EpAddr      of 'ty tmty
                                |   TmNOT       of 'ty tmty
                                |   EpValue
                                |   EpSender
                                |   EpThis
                                |   Balanc      of 'ty tmty

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

let rec str_of_tm  e         = match fst e with 
    | TmApp(t1,t2)              -> "TmApp(" ^ str_of_tm t1 ^ "," ^ str_of_tm t2 ^ ")"
    | TmAbs(x,tyX,t)            -> "(λ" ^ x ^ ":" ^ str_of_ty tyX ^ "." ^ str_of_tm t ^ ")"
    | TmFix(f,n,_,t)            -> "TmFix(λ" ^ f ^ " " ^ n ^ "→" ^ str_of_tm t ^ ")" 
    | TmId(s)                   -> "TmId(" ^ s ^ ")"
    | TmI(i,n)                  -> "TmI"      ^ str_of_int i 
    | TmIRec(i)                 -> "TmRec"      ^ str_of_int i 
    | TmIStrct(i)               -> "{Struct "   ^ str_of_int i ^ "}"
    | TmIf(b,t,t')              -> "(If " ^ str_of_tm b ^ " Then " ^ str_of_tm t ^ " Else " ^ str_of_tm t' ^ ")"
    | TmEQ(a,b)                 -> str_of_tm a ^ "==" ^ str_of_tm b
    | TmU256(b)                 -> "u256" ^ str_of_big b 
    | TmU8(b)                   -> "u8"   ^ str_of_big b 
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
    | TmSfDstr   _              -> "selfdestruct"
    | TmAssign(l,r)             -> "assignment" 
    | EpThis                    -> "this"
    | EpNow                     -> "now"
    | EpSender                  -> "sender"
    | EpValue                   -> "value"
    | TmNOT         _           -> "not"
    | TmNEQ         _           -> "neq"
    | TmLAND        _           -> "_ && _"
    | TmLT          _           -> "lt"
    | TmGT          _           -> "gt"
    | EpAddr        _           -> "address"
    | TmDeref       _           -> "dereference of ..."
    | Balanc        _           -> "balance" 

;;

let pr_tm t = ps (str_of_tm t)
let pe_tm t = pe (str_of_tm t) 

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
    | TyI(x,n)                -> onVar c x n
    | tyT                       -> tyT

let rec tmWalk onVar onType c   = let (f,g) = (onVar,onType) in function 
   (* | TmLet(x,t1,t2)            -> TmLet(x,tmWalk f g c t1, tmWalk f g(c+1)t2) *)
    | TmAbs(x,tyT,(t2,b))       -> TmAbs(x,g c tyT,(tmWalk f g(c+1)t2,b))
    | TmApp((t1,a),(t2,b))      -> TmApp((tmWalk f g c t1,a),(tmWalk f g c t2,b)) 
    | TmI(x,n)                -> onVar c x n
    | x                         -> x

let tyShiftOnVar d          = fun c x n  ->  if x>=c then TyI(x+d,n+d) else TyI(x,n+d)
let tyShiftAbove d          = tyWalk (tyShiftOnVar d) 
let tyShift d               = tyShiftAbove d 0    

let tmShiftOnVar d          = fun c x n  ->  if x>=c then TmI(x+d,n+d) else TmI(x,n+d)
let tmShiftAbove d          = tmWalk (tmShiftOnVar d) (tyShiftAbove d) 
let tmShift d               = tmShiftAbove d 0 

(*
let bindshift d             = function 
    | BindTyAbb(tyT)            ->  BindTyAbb(tyShift d tyT) 
    | BindTmI(tyT)            ->  BindTmI(tyShift d tyT) 
    | BindTmAbb(t,tyT_opt)      ->  (let tyT_opt' = match tyT_opt with 
                                        | None      -> None
                                        | Some(tyT) -> Some(tyShift d tyT) in 
                                    BindTmAbb(tmShift d t, tyT_opt'))
    | b                         ->  b
*)                                      
(* -------------------------------------------------- *) 
(* Substitution *) 
let tySubstOnVar j tyS tyT  = fun c x n -> if x=j+c then tyShift c tyS else TyI(x,n)
let tySubst      j tyS tyT  = tyWalk(tySubstOnVar j tyS tyT)0 tyT

let tmSubstOnVar j s t      = fun c x n -> if x=j+c then tmShift c s   else TmI( x, n) 
let tmSubst      j s t      = tmWalk (tmSubstOnVar j s t) (fun k x -> x) 0 t
let tmSubstTop     s t      = tmShift (-1) (tmSubst 0 (tmShift 1 s) t) 

