(* ret_cont := what a contract become after calling it *) 

open Big_int
open Printf
open Misc

module L    = List 
module BL   = BatList




type ty                 = TyVoid
                        | TyUint256             (* 256 bits *) 
                        | TyUint8               (*   8 bits *) 
                        | TyBytes32             (* 256 bits *) 
                        | TyAddr                (* 160 bits *) 
                        | TyBool 
                        | TyRef                 of ty list
                        | TyTuple               of ty list
                        | TyMap                 of ty * ty 
                        | TyCntrctArch        of string   (* type of [bid(...)] where bid is a cntrct *) 
                        | TyCntrctInstance    of string   (* type of [b] declared as [bid b] *) 

let rec string_of_ty    = function 
    | TyVoid                -> "void" 
    | TyUint256             -> "uint256"
    | TyUint8               -> "uint8" 
    | TyBytes32             -> "bytes32" 
    | TyAddr                -> "address" 
    | TyBool                -> "bool" 
    | TyRef              _  -> "ref" 
    | TyTuple            _  -> "tuple" 
    | TyMap(a,b)            -> "mapping" 
    | TyCntrctArch     s  -> "cntrctArch " ^ s
    | TyCntrctInstance s  -> "cntrctInstance " ^ s

type arg                = 
                        { ty                    : ty
                        ; id                    : string 
                        ; loc                   : SideEffect.eff_loc option
                        } 

type event_arg          =
                        { event_arg_body        : arg
                        ; event_arg_indexed     : bool
                        }

type event              =        
                        { event_name            : string
                        ; event_args            : event_arg list
                        }



(*****************************************)
(***      STATEMENTS & EXPRESSIONS     ***)
(*****************************************)

type 'ty fncall=
                        { call_head             : string
                        ; call_args             : ('ty expr) list   }

and  'ty msg_info       =
                        { msg_value_info        : 'ty expr option
                        ; msg_reentrance_info   : 'ty stmt list     }

and  'ty new_expr       =
                        { new_head              : string
                        ; new_args              : 'ty expr list
                        ; new_msg_info          : 'ty msg_info      }

and  'ty send_expr      =
                        { send_cntrct      : 'ty expr
                        ; send_mthd      : string option
                        ; send_args             : 'ty expr list
                        ; send_msg_info         : 'ty msg_info      }

and  'ty stmt           =
                        | SmAbort
                        | SmReturn            of 'ty return
                        | SmAssign            of 'ty lexpr * 'ty expr
                        | SmVarDecl           of 'ty varDecl
                        | SmIfThen            of 'ty expr * 'ty stmt list
                        | SmIfThenElse            of 'ty expr * 'ty stmt list * 'ty stmt list
                        | SmSelfDestruct      of 'ty expr
                        | SmExpr              of 'ty expr
                        | SmLog               of string * 'ty expr list * event option

and  'ty expr           = 'ty expr_tm * 'ty

and  'ty expr_tm        =
                        | EpTrue
                        | EpFalse
                        | EpDecLit256         of big_int
                        | EpDecLit8           of big_int
                        | EpNow
                        | EpFnCall           of 'ty fncall
                        | EpIdent             of string
                        | EpParen             of 'ty expr
                        | EpNew               of 'ty new_expr
                        | EpSend              of 'ty send_expr
                        | EpLand              of 'ty expr * 'ty expr
                        | EpLt                of 'ty expr * 'ty expr
                        | EpGt                of 'ty expr * 'ty expr
                        | EpNeq               of 'ty expr * 'ty expr
                        | EpEq                of 'ty expr * 'ty expr
                        | EpAddr              of 'ty expr
                        | EpNot               of 'ty expr
                        | EpArray       of 'ty lexpr
                        | EpValue
                        | EpSender
                        | EpThis
                        | EpSingleDeref       of 'ty expr
                        | EpTupleDeref        of 'ty expr
                        | EpPlus              of 'ty expr * 'ty expr
                        | EpMinus             of 'ty expr * 'ty expr
                        | EpMult              of 'ty expr * 'ty expr
                        | EpBalance           of 'ty expr

and 'ty lexpr           =
                        | LEpArray      of 'ty array

and 'ty array    =
                        { array_name    : 'ty expr
                        ; array_index    : 'ty expr          }

and 'ty varDecl     =
                        { varDecl_ty            : ty
                        ; varDecl_id            : string
                        ; varDecl_val           : 'ty expr          }

and 'ty return          =
                        { ret_expr              : 'ty expr option
                        ; ret_cont              : 'ty expr          }



let read_array   = function 
    | LEpArray a    -> a

let event_arg_of_arg arg isIndexed =
    { event_arg_body        = arg
    ; event_arg_indexed     = isIndexed       }

let arg_of_event_arg e  = e.event_arg_body

let split_event_args ev (args:'a expr list) =
    let indexed : bool list = L.map (fun a->a.event_arg_indexed) ev.event_args in
    let combined            = L.combine args indexed in
    let is,ns               = BL.partition snd combined in
    L.map fst is, L.map fst ns



(*****************************************)
(***    METHODs      &    CONTRACTS    ***)
(*****************************************)

type 'ty mthd_body      = 'ty stmt list

type mthd_info          =
                        { mthd_ret_ty       : ty list    (* empty or single type *) 
                        ; mthd_name         : string
                        ; mthd_args         : arg list   
                        }

type mthd_head          =      
                        | Method of mthd_info
                        | Default

type 'ty mthd           =
                        { mthd_head         :     mthd_head
                        ; mthd_body         : 'ty mthd_body
                        }


type 'ty cntrct         =
                        { cntrct_name       : string
                        ; cntrct_args       : arg list
                        ; mthds             : 'ty mthd list
                        }


(*****************************************)
(***           TOPLEVEL                ***)
(*****************************************)

type 'ty toplevel       =
                        | Cntrct        of 'ty cntrct
                        | Event         of event



let cntrct_name_of_ret_cont ((r,_):'ty expr) : string option = match r with
    | EpFnCall c             -> Some c.call_head
    | _                         -> None

let mthd_head_arg_list (h:mthd_head) : arg list = match h with
    | Method mthd               -> mthd.mthd_args
    | Default                   -> []

let cntrct_name_of_instance ((_,(t,_)):(ty*'a)expr) = match t with
    | TyCntrctInstance s        -> s
    | tyT                       -> err ("seeking cntrct_name_of non-cntrct "^(string_of_ty tyT))

let string_of_expr_inner = function 
    | EpThis                  -> "this"
    | EpArray _         -> "a[idx]"
    | EpSend _                -> "send"
    | EpNew _                 -> "new"
    | EpParen _               -> "()"
    | EpIdent str             -> "ident "^str
    | EpFnCall _             -> "call"
    | EpNow                   -> "now"
    | EpSender                -> "sender"
    | EpTrue                  -> "true"
    | EpFalse                 -> "false"
    | EpDecLit256 d           -> "declit "^(string_of_big_int d)
    | EpDecLit8 d             -> "declit "^(string_of_big_int d)
    | EpNot _                 -> "not"
    | EpNeq _                 -> "neq"
    | EpLand _                -> "_ && _"
    | EpLt _                  -> "lt"
    | EpGt _                  -> "gt"
    | EpValue                 -> "value"
    | EpEq _                  -> "equality"
    | EpAddr _                -> "address"
    | EpSingleDeref _         -> "dereference of ..."
    | EpTupleDeref _          -> "dereference of tuple..."
    | EpPlus (a, b)           -> "... + ..."
    | EpMinus (a, b)          -> "... - ..."
    | EpMult (a, b)           -> "... * ..."
    | EpBalance _             -> "balance"

let is_mapping = function 
    | TyUint256
    | TyUint8
    | TyBytes32
    | TyAddr
    | TyBool
    | TyRef _
    | TyTuple _
    | TyCntrctArch _
    | TyCntrctInstance _
    | TyVoid                    -> false
    | TyMap _                   -> true

let count_plain_args (args:ty list) = L.length (L.filter (not $ is_mapping) args)

let fits_in_one_stor_slot = function 
    | TyUint8
    | TyUint256
    | TyBytes32
    | TyAddr
    | TyBool
    | TyCntrctInstance _
    | TyMap _                   -> true
    | TyRef _     
    | TyTuple _        
    | TyCntrctArch _ 
    | TyVoid                    -> false

let size_of_ty (* in bytes *) = function
    | TyUint8                   ->  1
    | TyUint256                 -> 32
    | TyBytes32                 -> 32
    | TyAddr                    -> 20
    | TyCntrctInstance _        -> 20 (* address as word *)
    | TyBool                    -> 32
    | TyRef _                   -> 32
    | TyVoid                    -> err "size_of_ty VoidType"
    | TyTuple _                 -> err "size_of_ty TyTuple"
    | TyMap   _                 -> err "size_of_ty TyMap" 
    | TyCntrctArch     x        -> err ("size_of_ty TyCntrctArch: "^x)

let size_of_tys (tys:ty list)   = BL.sum (L.map size_of_ty tys)

let calldata_size_of_ty         = function 
    | TyMap _                   -> err "mapping cannot be a method arg"
    | TyRef _                   -> err "reference type cannot be a method arg"
    | TyTuple _                 -> err "tupletype not implemented"
    | TyCntrctArch _            -> err "CntrctArchType cannot be a method arg"
    | tyT                       -> size_of_ty tyT

let calldata_size_of_arg(arg:arg)   = calldata_size_of_ty arg.ty

let is_throw_only = function   (* :  ty stmt list -> bool *) 
    | []                        -> false
    | [SmAbort]               -> true
    | _                         -> false

let non_mapping_arg (arg:arg)   = match arg.ty with
    | TyMap _                   -> false
    | _                         -> true










let acceptable_as t0 t1     =   (t0 = t1) 
                            ||  ( match t0, t1 with
                                | TyAddr, TyCntrctInstance _ -> true
                                | _, _ -> false ) 

