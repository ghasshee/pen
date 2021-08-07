(* intf := interface *) 

open Misc
open Location
open Syntax
open Printf 
open Context

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

let hash_ty_mthd  m     = keccak_signature (string_of_tyMthd m)
let hash_of_evnt  e     = keccak_signature (string_of_evnt e)
let big_of_hex    h     = BB.big_int_of_string ("0x"^h)

