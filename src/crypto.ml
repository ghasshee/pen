(* intf := interface *) 

open Misc
open Location
open Syntax
open IndexList
open Printf 
open TypeEnv

module L  = List
module BL = BatList
module BS = BatString 
module BB = BatBig_int







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
    let name_of_mthd    = m.mthd_id                         in
    let args            = getArgTys m.mthd_args             in
    let arg_tys         = L.map snd args                    in
    let str_tys         = L.map string_of_ty arg_tys        in
    let ty              = String.concat "," str_tys         in
    name_of_mthd ^ "(" ^ ty ^ ")"

let string_of_evnt (ev:tyEvnt) =
    (* do I consider indexed no? *)
    let name            = ev.id                             in
    let args            = args_of_evnt_args ev.tyEvArgs     in 
    let argTys          = getArgTys args                    in
    let tys             = L.map snd argTys                  in
    let tyNames         = L.map string_of_ty tys            in
    let args            = String.concat "," tyNames         in
    name ^ "(" ^ args ^ ")"

let hash_ty_mthd  m     = keccak_signature (string_of_tyMthd m)
let hash_of_evnt  e     = keccak_signature (string_of_evnt e)
let big_of_hex    h     = BB.big_int_of_string ("0x"^h)

