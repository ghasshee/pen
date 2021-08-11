
open Misc
open Printf 

module S = String 
module B = BatBig_int
module R = Rope 



type hex                                =   R.t

let empty_hex                           =   R.empty
let concat_hex                          =   R.concat2
let string_of_hex ?prefix:(prefix="")h  =   R.to_string(concat_hex(R.of_string prefix)h)

let hex_of_big_int b len                =   let s           =   B.to_string_in_hexa b         in
                                            let char_len    =   2 * len                       in (* 1 char = 1/2 byte *) 
                                            if S.length s > char_len then err "hex_of_big_int: too big" ; 
                                            let prefix      =   S.make (char_len - S.length s) '0' in
                                            concat_hex(R.of_string prefix)(R.of_string s)

let pr_hex ?prefix:(prefix="")h         =   printf"%s\n"(string_of_hex ~prefix h)
let hex_of_string s                     =   R.of_string s  (* TODO: check if the string contains only 0-9a-fA-F *)
