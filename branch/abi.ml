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

let abi_string_of_ty       = function 
    | TyUint256                ->  "uint256" 
    | TyUint8                  ->  "uint8"   
    | TyBytes32               ->  "bytes32" 
    | TyAddr                  ->  "address"
    | TyBool                  ->  "bool"


let prABI_default_mthd   =
  "{\"type\":\"fallback\",\"inputs\": [],\"outputs\": [],\"payable\": true}"

let prABI_input         = function 
    TyVar(id,ty) ->  sprintf "{\"name\": \"%s\", \"type\": \"%s\"}" id (abi_string_of_ty ty)

let prABI_inputs (args:ty list) : string =
    let strings         = L.map prABI_input args in
    BS.concat "," strings

let prABI_output (ty:ty) : string =
    sprintf "{\"name\": \"\", \"type\": \"%s\"}"
                 (abi_string_of_ty ty)

let prABI_outputs (tys:ty list) : string =
    let strs = L.map prABI_output tys in
    BS.concat "," strs

let prABI_mthd_info (TyMthd(id,args,ret)) = 
    sprintf "{\"type\":\"function\",\"name\":\"%s\",\"inputs\": [%s],\"outputs\": [%s],\"payable\": true}"
        id (prABI_inputs args) (prABI_output ret)

let prABI_mthd (c:ty mthd) : string = match c.mthd_head with
    | TyMthd(id,args,ret) ->  prABI_mthd_info (TyMthd(id,args,ret))
    | TyDefault             ->  prABI_default_mthd

let prABI_cnstrctr (c:ty cntrct) : string =
    sprintf
        "{\"type\": \"constructor\", \"inputs\":[%s], \"name\": \"%s\", \"outputs\":[], \"payable\": true}"
        (prABI_inputs (L.filter non_mapping_arg c.fields )) (c.cn_id)

let prABI_cntrct seen_cnstrctr (c:ty cntrct) : string =
    let cases               =   c.mthds in
    let strs : string list  =   L.map prABI_mthd cases in
    let strs                =   if !seen_cnstrctr then strs
                                else prABI_cnstrctr c :: strs in
    let ()                  =   (seen_cnstrctr := true) in
    BS.concat "," strs


let prABI_evnt_arg = function TyEvVar(id,ty,visible) ->  
    sprintf "{\"name\":\"%s\",\"type\":\"%s\",\"indexed\":%s}"
                 id (abi_string_of_ty ty) (string_of_bool visible)

let prABI_evnt_inputs (is:ty list) : string =
    let strs : string list  = L.map prABI_evnt_arg is in
    BS.concat "," strs

let prABI_evnt = function TyEv(id,tyEvArgs) -> 
    sprintf "{\"type\":\"evnt\",\"inputs\":[%s],\"name\":\"%s\"}"
        (prABI_evnt_inputs tyEvArgs) id

let prABI_toplevel seen_cnstrctr (t:ty toplevel) : string = match t with
    | Cntrct c                  -> prABI_cntrct seen_cnstrctr c
    | Event e                   -> prABI_evnt e

let prABI (tops : ty toplevel idx_list) : unit =
    let seen_cnstrctr    = ref false in
    let ()                  = printf "[" in
    let strs : string list  = L.map (prABI_toplevel seen_cnstrctr) (values tops) in
    let ()                  = printf "%s" (BS.concat "," strs) in
    printf "]"
