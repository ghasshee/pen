
open Printf 

module S = String 
module B = BatBig_int
module R = Rope 



type hex                    = R.t

let empty_hex               = R.empty
let concat_hex              = R.concat2
let length_of_hex h         = R.length h / 2
let hex_of_big_int b len    =
    let raw                     =   B.to_string_in_hexa b           in
    let char_limit              =   2 * len                         in
    let()                       =   if S.length raw > char_limit 
                                        then failwith "hex_of_big_int: too big" in
    let missing_len             =   char_limit - S.length raw       in
    let prefix                  =   S.make missing_len '0'          in
    concat_hex(R.of_string prefix)(R.of_string raw)

let string_of_hex ?prefix:(prefix:string = "") (h:hex) : string =
    let ret                     = concat_hex (R.of_string prefix) h in
    R.to_string ret

let pr_hex ?prefix:(prefix="")h = printf"%s\n"(string_of_hex ~prefix h)

let hex_of_string s =
    (* TODO: check if the string contains only 0-9a-fA-F *)
    R.of_string s
