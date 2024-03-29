
open Misc
open Location
open Syntax
open Printf 
open Context
open Cryptokit

module BS   = BatString 
module Hash = Cryptokit.Hash
module Hexa = Cryptokit.Hexa

let sha3 = Hash.keccak 256 ;;


let hex s               = transform_string (Hexa.encode()) s 
let strip_0x h          =   if BS.starts_with h "0x" then BS.tail h 2 else h
    
let keccak_sig8 str     =   String.sub (hex (hash_string (Hash.keccak 256) str)) 0 8 

let hash_ty_mthd  m     =   keccak_sig8 (str_of_tyMthd m)
let hash_of_evnt  e     =   keccak_sig8 (str_of_evnt e)
let big_of_hex    h     =   big_of_str ("0x"^h)

