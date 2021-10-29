open Printf
open Misc

module L    = List 
module BL   = BatList


type ty           (* atomic *)  =   TyVoid                             
                  (* stomic *)  |   TyUnit             
                  (* atomic *)  |   TyU256              (* 256 bits *) 
                  (* atomic *)  |   TyU8                (*   8 bits *) 
                  (* atomic *)  |   TyBytes32           (* 256 bits *) 
                  (* atomic *)  |   TyAddr              (* 160 bits *) 
                  (* atomic *)  |   TyBool 
                  (* atomic *)  |   TyRef               of ty 
                  (* atomic *)  |   TyTuple             of ty list
                  (* atomic *)  |   TyMap               of ty * ty 
                  (* atomic *)  |   TyInstnc            of string                       (* type of [b] declared as [bid b]          *) 
                                |   TyMthd              of string * ty list * ty        (*  TyMthd(id, tyArgs, tyRet)               *)
                                |   TyDefault
                                |   TyAbs               of  ty * ty                     (*  TyAbs(tyArgs, tyRet)                    *)
                                |   TyIdx               of int * int                    
                                |   TyVar               of string * ty                  (*  TyVar(id, ty)                           *) 
                                |   TyEvVar             of string * ty * bool           (*  TyEvVar(id,ty,indexed)                  *)
                                |   TyEv                of string * ty list 
                                |   TyCn                of string * ty list * ty list   (*  TyCn(id,tyCnArgs,tyMethod list          *) 


let id_of_var (TyVar(id,_))     =   id 
let ty_of_var (TyVar(_,ty))     =   ty 
let tys_of_vars                 =   L.map ty_of_var 

let rec string_of_ty            =   function 
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
    | _                         ->  "undefined" 

let  arg_of_evnt_arg            =   function TyEvVar(id,ty,visible) -> TyVar(id,ty)
let args_of_evnt_args           =   L.map arg_of_evnt_arg

let split_evnt_args tyEv args   =   match tyEv with TyEv(id,tyEvArgs)  -> 
    let visibles : bool list    =   L.map (function TyEvVar(_,_,visible)->visible) tyEvArgs in
    let combined                =   L.combine args visibles in
    let is,ns                   =   BL.partition snd combined in
    L.map fst is, L.map fst ns

(*****************************************)
(***      STATEMENTS & EXPRESSIONS     ***)
(*****************************************)

type  'ty _new                  =   { new_id            : string
                                    ; new_args          : 'ty exprTy list
                                    ; new_msg           : 'ty exprTy        }

and  'ty stmt                   =   SmExpr              of 'ty exprTy
                                |   SmAssign            of 'ty expr * 'ty exprTy
                                |   SmDecl              of ty * string * 'ty exprTy 
                                |   SmIf                of 'ty exprTy * 'ty stmt list * 'ty stmt list

and  'ty exprTy                 =   'ty expr * 'ty

and  'ty expr                   =   
                                |   TmReturn            of 'ty exprTy * 'ty exprTy   (* TmReturn(ret,cont) *)  
                                |   TmId                of string
                                |   TmAbort 
                                |   TmSlfDstrct         of 'ty exprTy
                                |   TmLog               of string * 'ty exprTy list * ty option (* ty := TyEv *)
                                |   TmRef               of 'ty exprTy
                                |   TmDeref             of 'ty exprTy
                                |   TmAssign            of 'ty expr * 'ty exprTy 
                                |   TmLoc               of int 
                                |   TmIdx               of int * int 
                                |   TmIdxRec            of int 
                                |   TmIdxStrct          of int 
                                |   TmAbs               of string * ty * 'ty exprTy
                                |   TmApp               of 'ty exprTy * 'ty exprTy  
                                |   TmIf                of 'ty exprTy * 'ty exprTy * 'ty exprTy 
                                |   TmFix               of string * string * ty * 'ty exprTy
                                |   TmUnit 
                                |   TmZero 
                                |   TmCall              of string * 'ty exprTy list   (* TmCall(cnname, args) *) 
                                |   TmArray             of 'ty exprTy * 'ty exprTy    (* TmArray(id,idx) *) 
                                |   EpTrue
                                |   EpFalse
                                |   TmU256              of big
                                |   EpUint8             of big
                                |   EpNow
                                |   EpNew               of 'ty _new
                                |   TmSend              of 'ty exprTy * string option * 'ty exprTy list * 'ty exprTy (* TmSend(cn,Some mname, args, msg) *) 
                                |   EpLAnd              of 'ty exprTy * 'ty exprTy
                                |   EpLT                of 'ty exprTy * 'ty exprTy
                                |   EpGT                of 'ty exprTy * 'ty exprTy
                                |   TmEq                of 'ty exprTy * 'ty exprTy
                                |   EpNEq               of 'ty exprTy * 'ty exprTy
                                |   EpAddr              of 'ty exprTy
                                |   EpNot               of 'ty exprTy
                                |   EpValue
                                |   EpSender
                                |   EpThis
                                |   EpDeref             of 'ty exprTy
                                |   EpPlus              of 'ty exprTy * 'ty exprTy
                                |   TmMinus             of 'ty exprTy * 'ty exprTy
                                |   TmMul               of 'ty exprTy * 'ty exprTy
                                |   EpBalance           of 'ty exprTy

let get_ty  (_,ty)              =   ty
let get_tm  (x,_)               =   x


(*****************************************)
(***    METHODs      &    CONTRACTS    ***)
(*****************************************)

type 'ty mthd                   =   TmMthd              of ty * 'ty stmt list 

type 'ty cntrct                 =   { id                : string
                                    ; fields            : ty list
                                    ; mthds             : 'ty mthd list }

let filter_vars                 =   L.filter (function TyVar(_,TyMap _) -> false | TyVar _ -> true )
let filter_arrs                 =   L.filter (function TyVar(_,TyMap _) -> true  | TyVar _ -> false) 
let varTys_of_cn cn             =   filter_vars cn.fields
let arrTys_of_cn cn             =   filter_arrs cn.fields

let string_of_tyMthd (TyMthd(id,args,ret))  =
    let argTys          = tys_of_vars (filter_vars args)        in
    let strTys          = L.map string_of_ty argTys             in
    let tys             = S.concat "," strTys                   in
    id    ^ "(" ^ tys ^ ")"

let string_of_evnt  = function TyEv(id,tyEvArgs) -> 
    let args            = args_of_evnt_args tyEvArgs            in 
    let argTys          = tys_of_vars (filter_vars args)        in
    let strTys          = L.map string_of_ty argTys             in
    let tys             = S.concat "," strTys                   in
    id ^ "(" ^ tys ^ ")"

(*****************************************)
(***           TOPLEVEL                ***)
(*****************************************)

type 'ty toplevel               =   Cntrct        of 'ty cntrct
                                |   Event         of ty    (* ty = TyEv *) 

let filter_method               =   BL.filter_map (function   | TyDefault                   -> None 
                                                              | TyMthd(i,a,r)             -> Some (TyMthd(i,a,r)) )  
let default_exists              =   L.exists      (function   | TyDefault                   -> true
                                                              | TyMthd(_,_,_)             -> false  )

let cntrct_name_of_ret_cont     = function 
    | TmCall(id,args),_         -> Some id
    | _,_                       -> None

let argTys_of_mthd                = function 
    | TyMthd(_,argTys,_)        -> argTys
    | TyDefault                 -> []

let cntrct_name_of_instance     = function
    | _,(TyInstnc cn,_)        -> cn


(*****************************************)
(***           PRINTING                ***)
(*****************************************)

let rec string_of_expr          = function 
    | TmZero                    -> "O" 
    | TmFix(f,n,_,_)            -> "fix(" ^ f ^ ")"
    | TmApp((t1,_),(t2,_))      -> "(" ^ string_of_expr t1 ^ ")(" ^ string_of_expr t2 ^ ")" 
    | TmAbs(x,tyX,(t,_))        -> "λ" ^ x ^ ":" ^ string_of_ty tyX ^ "." ^ string_of_expr t 
    | TmIdx(i,n)                -> "x" ^ string_of_int i 
    | TmIdxRec(i)               -> "rec" ^ string_of_int i 
    | TmIdxStrct(i)             -> "{struct " ^ string_of_int i ^ "}"
    | TmIf((b,_),(t1,_),(t2,_)) -> "if " ^ string_of_expr b ^ " then " ^ string_of_expr t1 ^ " else " ^ string_of_expr t2  
    | TmAbort                   -> "abort" 
    | TmReturn((r,_),_)         -> "return " ^ string_of_expr r 
    | TmLog(_,_,_)              -> "log"
    | TmSlfDstrct _             -> "selfdestruct"
    | TmId        str           -> "id " ^ str
    | TmEq          _           -> "equality"
    | TmU256   d                -> "uint " ^ string_of_big d
    | TmUnit                    -> "()"
    | TmMinus ((a,_),(b,_))     -> string_of_expr a ^ " - " ^ string_of_expr b
    | TmMul   ((a,_),(b,_))     -> string_of_expr a ^ " * " ^ string_of_expr b
    | TmCall(id,args)           -> "call(" ^ id ^ ")" 
    | TmArray((id,_),(idx,_))   -> string_of_expr id ^ "[" ^ string_of_expr idx ^ "]" 
    | TmSend        _           -> "send"
    | EpThis                    -> "this"
    | EpNew         _           -> "new"
    | EpNow                     -> "now"
    | EpSender                  -> "sender"
    | EpTrue                    -> "true"
    | EpFalse                   -> "false"
    | EpUint8     d             -> "uint " ^ string_of_big d
    | EpNot         _           -> "not"
    | EpNEq         _           -> "neq"
    | EpLAnd        _           -> "_ && _"
    | EpLT          _           -> "lt"
    | EpGT          _           -> "gt"
    | EpValue                   -> "value"
    | EpAddr        _           -> "address"
    | EpDeref       _           -> "dereference of ..."
    | EpPlus  ((a,_),(b,_))     -> string_of_expr a ^ " + " ^ string_of_expr b
    | EpBalance     _           -> "balance"

let rec string_of_tm  e         = match fst e with 
    | TmApp(t1,t2)              -> "TmApp(" ^ string_of_tm t1 ^ "," ^ string_of_tm t2 ^ ")"
    | TmAbs(x,tyX,t)            -> "(λ" ^ x ^ ":" ^ string_of_ty tyX ^ "." ^ string_of_tm t ^ ")"
    | TmIdx(i,n)                -> "TmIdx" ^ string_of_int i 
    | TmIdxRec(i)               -> "TmRec" ^ string_of_int i 
    | TmIdxStrct(i)             -> "{Struct " ^ string_of_int i ^ "}"
    | TmIf(b,t,t')              -> "(If " ^ string_of_tm b ^ " Then " ^ string_of_tm t ^ " Else " ^ string_of_tm t' ^ ")"
    | TmFix(f,n,_,t)            -> "TmFix(λ" ^ f ^ " " ^ n ^ "→" ^ string_of_tm t ^ ")" 
    | TmEq(a,b)                 -> string_of_tm a ^ "==" ^ string_of_tm b
    | TmU256(b)                 -> string_of_big b 
    | TmId(str)                 -> str
    | TmMul(a,b)                -> string_of_tm a ^ " * " ^ string_of_tm b
    | TmMinus(a,b)              -> string_of_tm a ^ " - " ^ string_of_tm b 
    | TmUnit                    -> "()" 
    | TmZero                    -> "O" 
    | TmSend _                  -> "TmSend()" 
    | e                         -> string_of_expr e 


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

