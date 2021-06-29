(* intf := interface *) 

open Misc
open Abstract
open Syntax
open IndexedList
open Printf 

module L  = List
module BL = BatList
module BS = BatString 
module BB = BatBig_int



type tyMthd                 =
                            { tyRet       : ty 
                            ; name        : string
                            ; tyArgs      : ty list }

type argTy                  = string * ty 
type argArr                 = string * ty * ty 

let getTy_of_var var        = match var.ty with
    | TyMap (_,_)               -> None
    | _                         -> Some(var.id,var.ty)
let getTy_of_vars           = BL.filter_map getTy_of_var

let getArr_of_var var       = match var.ty with
    | TyMap(k,v)                ->  Some (var.id, k, v)
    | _                         ->  None
let getArr_of_cntrct cn     = BL.filter_map getArr_of_var (cn.cntrct_args)

let positions_of_argLens lens =
    let rec loop ret used = function 
        | []            ->  L.rev ret
        | alen::rest    ->  assert (alen>0 && alen<=32);
                            loop (used+32-alen::ret) (used+32) rest in 
    loop [] 4(* signature length *) lens

let argLocs_of_mthd m       = match m.mthd_head with
    | Default           ->  []
    | Method h          ->  let sizes       = L.map calldata_size_of_arg h.mthd_args in
                            let positions   = positions_of_argLens sizes in
                            let size_pos    = L.combine positions sizes in
                            let locations   = L.map (fun(o,s)->Calldata{calldata_start=o;calldata_size=s}) size_pos in
                            let names       = L.map (fun a -> a.id) h.mthd_args in
                            let locEnv      = L.combine names locations in
                            locEnv

let argTys_of_cntrct cn         =  getTy_of_vars cn.cntrct_args

let total_size_of_argTys tys    =   try     BL.sum (L.map size_of_ty tys) 
                                    with    Invalid_argument _          -> 0
let total_size_of_args args     =   try     BL.sum (L.map (fun arg-> size_of_ty arg.ty) args)
                                    with    Invalid_argument _          -> 0 





module Hash = Cryptokit.Hash


let string_keccak str       =
  let sha3_256                  = Hash.keccak 256           in
  let ()                        = sha3_256#add_string str   in
  let ret                       = sha3_256#result           in
  let tr                        = Cryptokit.Hexa.encode ()  in
  let ()                        = tr#put_string ret         in
  let ()                        = tr#finish                 in
  let ret                       = tr#get_string             in
  (* need to convert ret into hex *)
  ret

let strip_0x h              =   if BS.starts_with h "0x" then BS.tail h 2 else h

let add_hex sha3_256 h      =
    let h                       =   strip_0x h in
    let add_byte c              =   sha3_256#add_char c in
    let chars                   =   BS.explode h in
    let rec work                =   function         
        | []                        -> ()
        | [x]                       -> err "odd-length hex"
        | a :: b :: rest            -> add_byte (Hex.to_char a b); work rest in
    work chars

let hex_keccak h        =
    let sha3_256                = Hash.keccak 256           in
    let tr                      = Cryptokit.Hexa.encode()   in
    let ret                     = sha3_256#result           in
    let ()                      = add_hex sha3_256 h        in
    let ()                      = tr#put_string ret         in
    let ()                      = tr#finish                 in
    let ret                     = tr#get_string             in
    (* need to convert ret into hex *)
    ret

let keccak_signature str =  String.sub (string_keccak str) 0 8

let string_of_tyMthd m  =
    let name_of_mthd    = m.mthd_name                       in
    let args            = getTy_of_vars m.mthd_args         in
    let arg_tys         = L.map snd args                    in
    let str_tys         = L.map string_of_ty arg_tys        in
    let ty              = String.concat "," str_tys         in
    name_of_mthd ^ "(" ^ ty ^ ")"

let string_of_evnt e =
    (* do I consider indexed no? *)
    let name            = e.evnt_name                      in
    let args            = args_of_evnt_args e.evnt_args   in 
    let argTys          = getTy_of_vars args                in
    let tys             = L.map snd argTys                  in
    let tyNames         = L.map string_of_ty tys            in
    let args            = String.concat "," tyNames         in
    name ^ "(" ^ args ^ ")"

let hash_ty_mthd  m     = keccak_signature (string_of_tyMthd m)
let hash_of_evnt  e     = keccak_signature (string_of_evnt e)
let big_of_hex    h     = BB.big_int_of_string ("0x"^h)

