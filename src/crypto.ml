
open Misc
open Location
open Syntax
open Printf 
open Context

module BS   = BatString 
module Hash = Cryptokit.Hash
module Hexa = Cryptokit.Hexa

let sha3 = Hash.keccak 256 ;;

let sha3_of_str str       =
  let ()                        = sha3#add_string str   in
  let ret                       = sha3#result           in   
  (* ret is a raw hex data i.e. \00\ff\fa*)  
  let tr                        = Hexa.encode ()        in   
  let ()                        = tr#put_string ret     in
  let ()                        = tr#finish             in
  let ret                       = tr#get_string         in
  (* ret is the string representation of HEX i.e. "00fffa" *) 
  ret

let strip_0x h          =   if BS.starts_with h "0x" then BS.tail h 2 else h
    
let keccak_sig8 str     =  String.sub (sha3_of_str str) 0 8

let hash_ty_mthd  m     =   keccak_sig8 (str_of_tyMthd m)
let hash_of_evnt  e     =   keccak_sig8 (str_of_evnt e)
let big_of_hex    h     =   big_of_str ("0x"^h)

