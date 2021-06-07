open Ethereum

(*************************) 
(*      PRINT ABI        *) 
(*************************) 
open Location 
open Syntax
open IndexedList
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

let prABI_input (arg:arg) : string =
    sprintf "{\"name\": \"%s\", \"type\": \"%s\"}" (arg.id)
                 (abi_string_of_ty arg.ty)

let prABI_inputs (args:arg list) : string =
    let strings         = L.map prABI_input args in
    BS.concat "," strings

let prABI_output (ty:ty) : string =
    sprintf "{\"name\": \"\", \"type\": \"%s\"}"
                 (abi_string_of_ty ty)

let prABI_outputs (tys:ty list) : string =
    let strings = L.map prABI_output tys in
    BS.concat "," strings

let prABI_mthd_info u =
    sprintf "{\"type\":\"function\",\"name\":\"%s\",\"inputs\": [%s],\"outputs\": [%s],\"payable\": true}"
        (u.mthd_name) (prABI_inputs u.mthd_args) (prABI_outputs u.mthd_ret_ty)

let prABI_mthd (c:ty mthd) : string = match c.mthd_head with
    | Method u       ->  prABI_mthd_info u
    | Default         ->  prABI_default_mthd

let prABI_cnstrctr (c:ty cntrct) : string =
    sprintf
        "{\"type\": \"cnstrctr\", \"inputs\":[%s], \"name\": \"%s\", \"outputs\":[], \"payable\": true}"
        (prABI_inputs (L.filter non_mapping_arg c.cntrct_args)) (c.cntrct_name)

let prABI_cntrct seen_cnstrctr (c:ty cntrct) : string =
    let cases               =   c.mthds in
    let strs : string list  =   L.map prABI_mthd cases in
    let strs                =   if !seen_cnstrctr then strs
                                else prABI_cnstrctr c :: strs in
    let ()                  =   (seen_cnstrctr := true) in
    BS.concat "," strs


let prABI_event_arg (a:event_arg) : string =
    sprintf "{\"name\":\"%s\",\"type\":\"%s\",\"indexed\":%s}"
                 (a.event_arg_body.id)
                 (abi_string_of_ty (a.event_arg_body.ty))
                 (string_of_bool a.event_arg_indexed)

let prABI_event_inputs (is:event_arg list) : string =
    let strs : string list  = L.map prABI_event_arg is in
    BS.concat "," strs

let prABI_event (e:event) : string =
    sprintf "{\"type\":\"event\",\"inputs\":[%s],\"name\":\"%s\"}"
        (prABI_event_inputs e.event_args) (e.event_name)

let prABI_toplevel seen_cnstrctr (t:ty toplevel) : string = match t with
    | Cntrct c                -> prABI_cntrct seen_cnstrctr c
    | Event e                   -> prABI_event e

let prABI (tops : ty toplevel idx_list) : unit =
    let seen_cnstrctr    = ref false in
    let ()                  = printf "[" in
    let strs : string list  = L.map (prABI_toplevel seen_cnstrctr) (values tops) in
    let ()                  = printf "%s" (BS.concat "," strs) in
    printf "]"
