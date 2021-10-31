open Crypto

(*************************) 
(*      PRINT ABI        *) 
(*************************) 
open Location 
open Syntax
open Misc
open Printf 

module L  = List
module BL = BatList
module BS = BatString 
module BB = BatBig_int

let abi_str_of_ty       = function 
    | TyU256                ->  "uint256" 
    | TyU8                  ->  "uint8"   
    | TyBytes32               ->  "bytes32" 
    | TyAddr                  ->  "address"
    | TyBool                  ->  "bool"


let prABI_default_mthd   =
  "{\"type\":\"fallback\",\"inputs\": [],\"outputs\": [],\"payable\": true}"

let prABI_input         = function 
    TyVar(id,ty) ->  sprintf "{\"name\": \"%s\", \"type\": \"%s\"}" id (abi_str_of_ty ty)

let prABI_inputs (args:ty list) : str =
    let strs         = L.map prABI_input args in
    BS.concat "," strs

let prABI_output (ty:ty) : str =
    sprintf "{\"name\": \"\", \"type\": \"%s\"}"
                 (abi_str_of_ty ty)

let prABI_outputs (tys:ty list) : str =
    let strs = L.map prABI_output tys in
    BS.concat "," strs

let prABI_mthd_info (TyMthd(id,args,ret)) = 
    sprintf "{\"type\":\"function\",\"name\":\"%s\",\"inputs\": [%s],\"outputs\": [%s],\"payable\": true}"
        id (prABI_inputs args) (prABI_output ret)

let prABI_mthd  = function 
    | TmMthd(TyDefault,_)       ->  prABI_default_mthd
    | TmMthd(tyM,_)             ->  prABI_mthd_info tyM

let prABI_cnstrctr (TmCn(id,flds,_)) =
    sprintf
        "{\"type\": \"constructor\", \"inputs\":[%s], \"name\": \"%s\", \"outputs\":[], \"payable\": true}"
        (prABI_inputs (L.filter non_mapping_arg flds)) id

let prABI_cntrct seen_cnstrctr (TmCn(id,flds,mthds)) = 
    let strs : str list  =   L.map prABI_mthd mthds in
    let strs                =   if !seen_cnstrctr then strs else prABI_cnstrctr (TmCn(id,flds,mthds)) :: strs in
    seen_cnstrctr := true; 
    BS.concat "," strs


let prABI_evnt_arg = function TyEvVar(id,ty,visible) ->  
    sprintf "{\"name\":\"%s\",\"type\":\"%s\",\"indexed\":%s}"
                 id (abi_str_of_ty ty) (str_of_bool visible)

let prABI_evnt_inputs (is:ty list) : str =
    let strs : str list  = L.map prABI_evnt_arg is in
    BS.concat "," strs

let prABI_evnt = function TyEv(id,tyEvArgs) -> 
    sprintf "{\"type\":\"evnt\",\"inputs\":[%s],\"name\":\"%s\"}"
        (prABI_evnt_inputs tyEvArgs) id

let prABI_toplevel seen_cnstrctr = function 
    | TmCn(id,fs,ms)           -> prABI_cntrct seen_cnstrctr (TmCn(id,fs,ms))
    | TmEv e                   -> prABI_evnt e

let prABI (tops : ty toplevel idxlist) : unit =
    let seen_cnstrctr    = ref false in
    let ()                  = printf "[" in
    let strs : str list  = L.map (prABI_toplevel seen_cnstrctr) (values tops) in
    let ()                  = printf "%s" (BS.concat "," strs) in
    printf "]"
