(* intf := interface *) 

open Location 
open Syntax
open IndexedList
open Printf 

module L  = List
module BL = BatList
module BS = BatString 
module BB = BatBig_int


let err                     = failwith 
let word_bits               = 256
let sig_bits                = 32


type tyMthd                 =
                            { tyRet       : ty list
                            ; name        : string
                            ; tyArgs      : ty list }


type argTy                  = string * ty 
type argArr                 = string * ty * ty 

let getTy_of_arg arg        = match arg.ty with
    | TyMap (_,_)               -> None
    | _                         -> Some(arg.id,arg.ty)

let getTy_of_args           = BL.filter_map getTy_of_arg

let getArr_of_arg arg       = match arg.ty with
    | TyMap(k,v)                ->  Some (arg.id, k, v)
    | _                         ->  None

let getArr_of_cntrct cn     = BL.filter_map getArr_of_arg (cn.cntrct_args)

let positions_of_argLens lens =
    let rec loop ret used = function 
        | []            ->  L.rev ret
        | alen::rest    ->  assert (alen>0 && alen<=32);
                            loop (used+32-alen::ret) (used+32) rest in 
    loop [] 4(* signature length *) lens

let pr_argLoc r             = L.iter (fun(nm,loc) -> printf"arg %s at %s\n" nm (string_of_location loc)) r

let argLocs_of_mthd m       = match m.mthd_head with
    | Default           ->  []
    | Method h          ->  let sizes       = L.map calldata_size_of_arg h.mthd_args in
                            let positions   = positions_of_argLens sizes in
                            let size_pos    = L.combine positions sizes in
                            let locations   = L.map (fun(o,s)->Calldata{calldata_start=o;calldata_size=s}) size_pos in
                            let names       = L.map (fun a -> a.id) h.mthd_args in
                            let locEnv      = L.combine names locations in
                            locEnv


let cnstrctr_args (cn:ty cntrct) : (string*ty) list = 
    getTy_of_args cn.cntrct_args

let total_size_of_tyArgs tys = 
    try     BL.sum (L.map size_of_ty tys) 
    with    Invalid_argument _          -> 0

let total_size_of_args args = 
    try     BL.sum (L.map (fun arg-> size_of_ty arg.ty) args)
    with    Invalid_argument _          -> 0 







module Hash = Cryptokit.Hash


let string_keccak str : string=
  let sha3_256                      = Hash.keccak 256 in
  let ()                            = sha3_256#add_string str in
  let ret                           = sha3_256#result in
  let tr                            = Cryptokit.Hexa.encode () in
  let ()                            = tr#put_string ret in
  let ()                            = tr#finish in
  let ret                           = tr#get_string in
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

let hex_keccak h : string   =
    let sha3_256                = Hash.keccak 256           in
    let tr                      = Cryptokit.Hexa.encode()   in
    let ret                     = sha3_256#result           in
    let ()                      = add_hex sha3_256 h        in
    let ()                      = tr#put_string ret         in
    let ()                      = tr#finish                 in
    let ret                     = tr#get_string             in
    (* need to convert ret into hex *)
    ret

let keccak_signature str : string  =  String.sub (string_keccak str) 0 8

let string_of_mthd_info_ty m =
    let name_of_mthd    = m.mthd_name                       in
    let args            = getTy_of_args m.mthd_args          in
    let arg_tys         = L.map snd args                    in
    let str_tys         = L.map string_of_ty arg_tys   in
    let ty              = String.concat "," str_tys         in
    name_of_mthd ^ "(" ^ ty ^ ")"

(* XXX: refactor with the above function *)
let string_of_event e =
    (* do I consider indexed no? *)
    let name            = e.event_name in
    let args            = getTy_of_args (L.map arg_of_event_arg e.event_args) in
    let arg_tys         = L.map snd args in
    let list_of_tys     = L.map string_of_ty arg_tys in
    let args            = String.concat "," list_of_tys in
    name ^ "(" ^ args ^ ")"

let hash_of_mthd_info_ty m =
    let s               = string_of_mthd_info_ty m in
    keccak_signature s

let event_sig_hash e =
    let sign            = string_of_event e in
    keccak_signature sign


let hex_to_big_int h    = BB.big_int_of_string ("0x"^h)




