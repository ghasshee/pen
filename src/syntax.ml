open Big_int
open Printf
open Misc

module L    = List 
module BL   = BatList

type info = int
type ty'     =
    | TyVar     of int * int 
    | TyId      of string 
    | TyTop
    | TyRef     of ty' 
    | TyVariant of (string * ty') list 
    | TyRecord  of (string * ty') list 
    | TyArr     of ty' * ty'
    | TyList    of ty' 
    | TyFloat 
    | TyString  
    | TyUnit
    | TyBool
    | TyNat 
;;

type term =
    (* Ref *)
    | TmRef         of info * term 
    | TmDeref       of info * term 
    | TmLoc         of info * int 
    | TmAssign      of info * term * term 
    (* List *)
    | TmNil         of info * ty'
    | TmCons        of info * ty' * term * term 
    | TmIsNil       of info * ty' * term 
    | TmHead        of info * ty' * term 
    | TmTail        of info * ty' * term 
    (* Fix *)
    | TmFix         of info * term 
    (* Float / String  *) 
    | TmString      of info * string 
    | TmFloat       of info * float
    | TmTimesfloat  of info * term * term 
    (* Variant *)
    | TmTag         of info * string * term * ty'
    | TmCase        of info * term * (string * (string * term)) list  (* <- (label*(variable*term) list *) 
    (* Record *)
    | TmProj        of info * term * string  
    | TmRecord      of info * (string * term) list 
    (* Ascription *) 
    | TmAscribe     of info * term * ty'
    (* Unit *)
    | TmUnit        of info 
    (* Let  *)
    | TmLet         of info * string * term * term
    (* Lambda *) 
    | TmVar         of info * int * int 
    | TmAbs         of info * string * ty' * term 
    | TmApp         of info * term * term 
    (* Arith *) 
    | TmZero        of info
    | TmSucc        of info * term
    | TmPred        of info * term
    | TmIsZero      of info * term
    (* Bool *) 
    | TmTrue        of info
    | TmFalse       of info
    | TmIf          of info * term * term * term

type ty           (* atomic *)  =   TyUint256           (* 256 bits *) 
                  (* atomic *)  |   TyUint8             (*   8 bits *) 
                  (* atomic *)  |   TyBytes32           (* 256 bits *) 
                  (* atomic *)  |   TyAddr              (* 160 bits *) 
                  (* atomic *)  |   TyBool 
                  (* atomic *)  |   TyRef               of ty 
                  (* atomic *)  |   TyTuple             of ty list
                  (* atomic *)  |   TyMap               of ty * ty 
                  (* atomic *)  |   TyCntrct            of string   (* type of [bid(...)] where bid is a cntrct *) 
                  (* atomic *)  |   TyInstnce           of string   (* type of [b] declared as [bid b] *) 
                                |   TyMthd              of string * ty list * ty        (*  TyMthd(id, tyArgs, tyRet)             *)
                                |   TyAbs               of          ty list * ty        (*  TyAbs(tyArgs, tyRet)                 *)
                                |   TyVar               of string * ty                  (*  (id, ty)                        *) 

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
    | TyCntrct      s           ->  "contract arch "     ^ s
    | TyInstnce     s           ->  "contract instance " ^ s

type tyEvntArg                  =   { arg               : ty  (* TyVar Only *) 
                                    ; indexed           : bool              }

type tyEvnt                     =   { id                : string
                                    ; tyEvArgs          : tyEvntArg list    }

type tyCntrct                   =   { id                : string   
                                    ; tyCnArgs          : ty list
                                    ; tyCnMthds         : ty list           } 

let tyEvntArg_of_arg arg isIdxd =   { arg               = arg
                                    ; indexed           = isIdxd            }
    
let  arg_of_evnt_arg e          =   e.arg
let args_of_evnt_args           =   L.map arg_of_evnt_arg

let split_evnt_args tyEv args =
    let indexed : bool list     =   L.map (fun arg->arg.indexed) tyEv.tyEvArgs in
    let combined                =   L.combine args indexed in
    let is,ns                   =   BL.partition snd combined in
    L.map fst is, L.map fst ns

(*****************************************)
(***      STATEMENTS & EXPRESSIONS     ***)
(*****************************************)

type 'ty _call                  =   { call_id           : string
                                    ; call_args         : ('ty exprTy) list    }
                                
and  'ty _new                   =   { new_id            : string
                                    ; new_args          : 'ty exprTy list
                                    ; new_msg           : 'ty exprTy              }
                                
and  'ty _send                  =   { sd_cn             : 'ty exprTy
                                    ; sd_mthd           : string option
                                    ; sd_args           : 'ty exprTy list
                                    ; sd_msg            : 'ty exprTy              }

and  'ty stmt                   =   SmAbort
                                |   SmReturn            of 'ty return
                                |   SmAssign            of 'ty lexpr * 'ty exprTy
                                |   SmDecl              of 'ty decl
                                (*|   SmIfThen            of 'ty exprTy * 'ty stmt list
                                |   SmIf                of 'ty exprTy * 'ty stmt list * 'ty stmt list *) 
                                |   SmSlfDstrct         of 'ty exprTy
                                |   SmExpr              of 'ty exprTy
                                |   SmLog               of string   * 'ty exprTy list * tyEvnt option

and  'ty exprTy                =   'ty expr * 'ty

and  'ty expr                   =   EpParen             of 'ty exprTy
                                |   TmVar               of int * int 
                                |   TmAbs               of string * ty * 'ty exprTy
                                |   TmApp               of 'ty exprTy * 'ty exprTy  
                                |   TmIf                of 'ty exprTy * 'ty exprTy * 'ty exprTy 
                                |   TmFix               of 'ty exorTy
                                |   EpTrue
                                |   EpFalse
                                |   EpDecLit256         of big_int
                                |   EpDecLit8           of big_int
                                |   EpNow
                                |   EpIdent             of string
                                |   EpFnCall            of 'ty _call
                                |   EpNew               of 'ty _new
                                |   EpSend              of 'ty _send
                                |   EpLAnd              of 'ty exprTy * 'ty exprTy
                                |   EpLT                of 'ty exprTy * 'ty exprTy
                                |   EpGT                of 'ty exprTy * 'ty exprTy
                                |   EpNeq               of 'ty exprTy * 'ty exprTy
                                |   EpEq                of 'ty exprTy * 'ty exprTy
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

and 'ty array                   =   { arrId             : 'ty exprTy
                                    ; arrIndex          : 'ty exprTy          }

and 'ty decl                    =   { declTy            : ty
                                    ; declId            : string
                                    ; declVal           : 'ty exprTy          }

and 'ty return                  =   { ret_expr          : 'ty exprTy option
                                    ; ret_cont          : 'ty exprTy          }
                                

(*****************************************)
(***    METHODs      &    CONTRACTS    ***)
(*****************************************)

type 'ty mthd_body              =   'ty stmt list

type mthd_info                  =   { mthd_id           : string
                                    ; mthd_args         : ty list    
                                    ; mthd_retTy        : ty           }
                                 
type mthd_head                  =   Method of mthd_info
                                |   Default

type 'ty mthd                   =   { mthd_head         :     mthd_head
                                    ; mthd_body         : 'ty mthd_body }
                                
type 'ty cntrct                 =   { cntrct_id         : string
                                    ; cntrct_args       : ty list
                                    ; mthds             : 'ty mthd list }


(*****************************************)
(***           TOPLEVEL                ***)
(*****************************************)

type 'ty toplevel               =   Cntrct        of 'ty cntrct
                                |   Event         of tyEvnt

let filter_usualMthd            =   BL.filter_map (function   | Default   -> None 
                                                              | Method m  -> Some m )  
let default_exists              =   L.exists      (function   | Default   -> true
                                                              | _         -> false  )

let cntrct_name_of_ret_cont     = function 
    | EpFnCall c,_              -> Some c.call_id
    | _,_                       -> None

let args_of_mthd                = function 
    | Method m                  -> m.mthd_args
    | Default                   -> []

let cntrct_name_of_instance     = function
    | _,(TyInstnce s,_)         -> s
    | _,(tyT,_)                 -> err ("seeking cntrct_name_of non-cntrct "^(string_of_ty tyT))

let string_of_expr_inner        = function 
    | EpThis                    -> "this"
    | EpArray       _           -> "a[idx]"
    | EpSend        _           -> "send"
    | EpNew         _           -> "new"
    | EpParen       _           -> "()"
    | EpIdent     str           -> "ident "^str
    | EpFnCall      _           -> "call"
    | EpNow                     -> "now"
    | EpSender                  -> "sender"
    | EpTrue                    -> "true"
    | EpFalse                   -> "false"
    | EpDecLit256   d           -> "declit "^(string_of_big_int d)
    | EpDecLit8     d           -> "declit "^(string_of_big_int d)
    | EpNot         _           -> "not"
    | EpNeq         _           -> "neq"
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

let is_mapping                  = function 
    | TyUint256
    | TyUint8
    | TyBytes32
    | TyAddr
    | TyBool
    | TyRef         _
    | TyTuple       _
    | TyCntrct      _
    | TyInstnce     _           -> false
    | TyMap         _           -> true

let count_plain_args            = L.length $ (L.filter (not $ is_mapping)) 

let fits_in_one_stor_slot       = function 
    | TyUint8
    | TyUint256
    | TyBytes32
    | TyAddr
    | TyBool
    | TyInstnce _
    | TyMap _                   -> true
    | TyRef _     
    | TyTuple _        
    | TyCntrct _                -> false

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
    | TyCntrct     x            -> err("size_of_ty TyCntrct: "^x)

let size_of_tys                 = BL.sum $ (L.map size_of_ty) 

let calldata_size_of_ty         = function 
    | TyMap _                   -> err "mapping cannot be a method arg"
    | TyRef _                   -> err "reference type cannot be a method arg"
    | TyTuple _                 -> err "tupletype not implemented"
    | TyCntrct _                -> err "abstract cntrct type cannot be a method arg"
    | tyT                       -> size_of_ty tyT

let calldata_size_of_arg (TyVar(_,ty))    = calldata_size_of_ty ty

let is_throw_only               = function   
    | [SmAbort]                 -> true
    | _                         -> false

let non_mapping_arg             = function 
    | TyVar(_,TyMap _)          -> false
    | TyVar(_,_)                -> true 
    | _ -> err "not an arg"

let acceptable_as t0 t1     =   ( t0 = t1 )  ||  ( match t0, t1 with
                                | TyAddr, TyInstnce _   -> true
                                | _     , _             -> false ) 
