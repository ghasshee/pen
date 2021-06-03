(* ret_cont := what a contract become after calling it *) 

open Big_int
open Printf
open Misc

module L    = List 
module BL   = BatList


let err = failwith 


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

type 'ty fn_call=
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
                        | AbortStmt
                        | ReturnStmt            of 'ty return
                        | AssignStmt            of 'ty lexpr * 'ty expr
                        | VarDeclStmt           of 'ty varDecl
                        | IfThenOnly            of 'ty expr * 'ty stmt list
                        | IfThenElse            of 'ty expr * 'ty stmt list * 'ty stmt list
                        | SelfDestructStmt      of 'ty expr
                        | ExprStmt              of 'ty expr
                        | LogStmt               of string * 'ty expr list * event option

and  'ty expr           = 'ty expr_tm * 'ty

and  'ty expr_tm        =
                        | TrueExpr
                        | FalseExpr
                        | DecLit256Expr         of big_int
                        | DecLit8Expr           of big_int
                        | NowExpr
                        | FunCallExpr           of 'ty fn_call
                        | IdentExpr             of string
                        | ParenExpr             of 'ty expr
                        | NewExpr               of 'ty new_expr
                        | SendExpr              of 'ty send_expr
                        | LandExpr              of 'ty expr * 'ty expr
                        | LtExpr                of 'ty expr * 'ty expr
                        | GtExpr                of 'ty expr * 'ty expr
                        | NeqExpr               of 'ty expr * 'ty expr
                        | EqExpr                of 'ty expr * 'ty expr
                        | AddrExpr              of 'ty expr
                        | NotExpr               of 'ty expr
                        | ArrayExpr       of 'ty lexpr
                        | ValueExpr
                        | SenderExpr
                        | ThisExpr
                        | SingleDerefExpr       of 'ty expr
                        | TupleDerefExpr        of 'ty expr
                        | PlusExpr              of 'ty expr * 'ty expr
                        | MinusExpr             of 'ty expr * 'ty expr
                        | MultExpr              of 'ty expr * 'ty expr
                        | BalanceExpr           of 'ty expr

and 'ty lexpr           =
                        | ArrayLExpr      of 'ty array

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
    | ArrayLExpr a    -> a

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
    | FunCallExpr c             -> Some c.call_head
    | _                         -> None

let mthd_head_arg_list (h:mthd_head) : arg list = match h with
    | Method mthd               -> mthd.mthd_args
    | Default                   -> []

let cntrct_name_of_instance ((_,(t,_)):(ty*'a)expr) = match t with
    | TyCntrctInstance s        -> s
    | tyT                       -> err ("seeking cntrct_name_of non-cntrct "^(string_of_ty tyT))

let string_of_expr_inner = function 
    | ThisExpr                  -> "this"
    | ArrayExpr _         -> "a[idx]"
    | SendExpr _                -> "send"
    | NewExpr _                 -> "new"
    | ParenExpr _               -> "()"
    | IdentExpr str             -> "ident "^str
    | FunCallExpr _             -> "call"
    | NowExpr                   -> "now"
    | SenderExpr                -> "sender"
    | TrueExpr                  -> "true"
    | FalseExpr                 -> "false"
    | DecLit256Expr d           -> "declit "^(string_of_big_int d)
    | DecLit8Expr d             -> "declit "^(string_of_big_int d)
    | NotExpr _                 -> "not"
    | NeqExpr _                 -> "neq"
    | LandExpr _                -> "_ && _"
    | LtExpr _                  -> "lt"
    | GtExpr _                  -> "gt"
    | ValueExpr                 -> "value"
    | EqExpr _                  -> "equality"
    | AddrExpr _                -> "address"
    | SingleDerefExpr _         -> "dereference of ..."
    | TupleDerefExpr _          -> "dereference of tuple..."
    | PlusExpr (a, b)           -> "... + ..."
    | MinusExpr (a, b)          -> "... - ..."
    | MultExpr (a, b)           -> "... * ..."
    | BalanceExpr _             -> "balance"

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
    | TyUint256
    | TyUint8
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
    | TyUint256                 -> 32
    | TyUint8                   -> 1
    | TyBytes32                 -> 32
    | TyAddr                    -> 20
    | TyBool                    -> 32
    | TyRef _                   -> 32
    | TyTuple lst               -> err "size_of_ty TyTuple"
    | TyMap     _               -> err "size_of_ty TyMap" (* XXX: this is just 32 I think *)
    | TyCntrctArch x            -> err ("size_of_ty TyCntrctArch: "^x)
    | TyCntrctInstance _        -> 20 (* address as word *)
    | TyVoid                    -> err "size_of_ty VoidType should not be asked"

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
    | [AbortStmt]               -> true
    | _                         -> false

let non_mapping_arg (arg:arg)   = match arg.ty with
    | TyMap _                   -> false
    | _                         -> true




(* MIGHT BECOME what ? *) 
(* update cntrcts or methods ?  *) 
(* method name search walking *) 
    
let rec functioncall_might_become f =
    L.concat (L.map expr_might_become f.call_args)

and new_expr_might_become n =
    L.concat (L.map expr_might_become n.new_args)@
        (msg_info_might_become n.new_msg_info)

and msg_info_might_become m = (match m.msg_value_info with
    | None                      -> []
    | Some e                    -> expr_might_become e)@
        [(* TODO: msg_reentrance_info should contain a continuation! *)]

and send_expr_might_become s =
    (expr_might_become s.send_cntrct)@
        (L.concat (L.map expr_might_become s.send_args))@
            (msg_info_might_become s.send_msg_info)

and array_might_become aa =
    expr_might_become aa.array_index

and expr_might_become e : string list = match fst e with
    | TrueExpr                  -> []
    | FalseExpr                 -> []
    | DecLit256Expr _           -> []
    | DecLit8Expr _             -> []
    | NowExpr                   -> []
    | IdentExpr _               -> []
    | FunCallExpr f             -> functioncall_might_become f
    | ParenExpr content         -> expr_might_become content
    | NewExpr n                 -> new_expr_might_become n
    | SendExpr s                -> send_expr_might_become s
    | LandExpr (l, r)           -> (expr_might_become l)@(expr_might_become r)
    | LtExpr (l, r)             -> (expr_might_become l)@(expr_might_become r)
    | GtExpr (l, r)             -> (expr_might_become l)@(expr_might_become r)
    | NeqExpr (l, r)            -> (expr_might_become l)@(expr_might_become r)
    | EqExpr (l, r)             -> (expr_might_become l)@(expr_might_become r)
    | AddrExpr a                -> expr_might_become a
    | NotExpr n                 -> expr_might_become n
    | ArrayExpr aa        -> lexpr_might_become aa
    | ValueExpr                 -> []
    | SenderExpr                -> []
    | ThisExpr                  -> []
    | SingleDerefExpr e         -> expr_might_become e
    | TupleDerefExpr  e         -> expr_might_become e
    | MinusExpr (a, b)
    | MultExpr (a, b)
    | PlusExpr (a, b)           -> (expr_might_become a)@(expr_might_become b)
    | BalanceExpr a             -> expr_might_become a


and lexpr_might_become      = function 
    | ArrayLExpr aa       -> array_might_become aa

let varDecl_might_become v  =
    expr_might_become v.varDecl_val

let rec stmt_might_become (s:ty stmt) : string list = match s with
    | AbortStmt                 -> []
    | ReturnStmt ret            -> (match ret.ret_expr with
        | Some e    -> expr_might_become e
        | None      -> []   ) @ (expr_might_become ret.ret_cont)@
                        (match cntrct_name_of_ret_cont ret.ret_cont with
                         | Some name    -> [name]
                         | None         -> [] )
    | AssignStmt(l,r)           -> (lexpr_might_become l)@(expr_might_become r)
    | VarDeclStmt v             -> varDecl_might_become v
    | IfThenOnly(c,block)       -> (expr_might_become c)@(stmts_might_become block)
    | IfThenElse(c,b0,b1)       -> (expr_might_become c)@(stmts_might_become b0)@(stmts_might_become b1)
    | SelfDestructStmt e        -> expr_might_become e
    | ExprStmt e                -> expr_might_become e
    | LogStmt(_,lst,_)          -> exprs_might_become lst

and exprs_might_become (l:ty expr list) : string list =
    L.concat (L.map expr_might_become l)

and stmts_might_become ss = L.concat (L.map stmt_might_become ss)

let mthd_might_become (m:ty mthd) : string list =
    let body = m.mthd_body in
    L.concat (L.map stmt_might_become body)

let might_become (c : ty cntrct) : string list =
    let mthds = c.mthds in
    L.concat (L.map mthd_might_become mthds)




(* LOOKUP_USUAL_METHOD *) 

let lookup_mthd_info_in_cntrct c name =
    let mthds = c.mthds in
    let mthd  = L.filter (fun c -> match c.mthd_head with
                          | Default  -> false
                          | Method m -> m.mthd_name=name) mthds in
    match mthd with
    | []        ->  raise Not_found
    | _::_::_   ->  eprintf "method %s duplicated\n%!" name;err "mthd_lookup" 
    | [a]       ->  begin match a.mthd_head with
                    | Method m      -> m
                    | Default       -> err "lookup_mthd_info_in_cntrct: default method found" end


let rec lookup_mthd_info_inner (already_seen:ty cntrct list) c mthd_name f : mthd_info =
    if L.mem c already_seen then raise Not_found else
    try  lookup_mthd_info_in_cntrct c mthd_name
    with Not_found ->   let already_seen = c::already_seen in
                        let becomes      = L.map f (might_become c) in
                        let rec try_becomes bs already_seen =
                          (match bs with
                           | []      -> raise Not_found
                           | h::tl   -> (try lookup_mthd_info_inner already_seen h mthd_name f
                                         with Not_found ->   let already_seen = h::already_seen in
                                                             try_becomes tl already_seen)) in
                        try_becomes becomes already_seen

let lookup_mthd_info c mthd_name f : mthd_info =
    lookup_mthd_info_inner [] c mthd_name f



    

let acceptable_as t0 t1 =
    (t0 = t1) || match t0, t1 with
    | TyAddr, TyCntrctInstance _ -> true
    | _, _ -> false
