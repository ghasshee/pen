(* intf := interface *) 

open Location 
open Syntax
open ContractId
open Printf 

module L  = List
module BL = BatList
module BS = BatString 
module BB = BatBig_int

let word_bits               = 256
let sig_bits                = 32

type intf_ty                =
                            | IntfUint       of int
                            | IntfBytes      of int
                            | IntfAddr
                            | IntfBool

type intf_arg               = string * intf_ty

(** [intf_ty_of_ty] parses "uint" into IntfUint 256, etc. *)
let intf_ty_of_ty       = function 
    | TyUint256                 -> IntfUint 256
    | TyUint8                   -> IntfUint   8
    | TyBytes32                 -> IntfBytes 32
    | TyAddr                    -> IntfAddr
    | TyBool                    -> IntfBool
    | TyContractInstance _      -> IntfAddr
    | TyTuple _                 -> failwith "intf_ty_of_type: tupleType not supported yet"
    | TyMap(_, _)               -> failwith "intf_ty_of_type: mappingType not supported"
    | TyContractArch _          -> failwith "contract arch-type does not appear in the ABI"
    | TyRef _                   -> failwith "reference type does not appear in the ABI"
    | TyVoid                    -> failwith "VoidType should not appear in the ABI"
    

let to_ty                  = function  
    | IntfUint x                -> if(x<0||x>256)then failwith "integer: out of range"; TyUint256
    | IntfBytes x when x=32     -> TyBytes32
    | IntfBool                  -> TyBool
    | IntfAddr                  -> TyAddr
    

let intf_ty_size            = function 
    | IntfUint _                -> 32 (* bytes *) 
    | IntfAddr                  -> 32 
    | IntfBool                  -> 32
    | IntfBytes _               -> 32








type function_signature     =
                            { sig_return  : intf_ty list
                            ; sig_name    : string
                            ; sig_args    : intf_ty list }





let get_intf_ty (arg:arg) : (string*intf_ty)option  = match (arg.ty) with
    | TyMap (_,_)               -> None
    | _                         -> Some(arg.id,intf_ty_of_ty (arg.ty))

let get_intf_tys : arg list -> (string*intf_ty)list = BL.filter_map get_intf_ty

let rec arg_sizes_to_positions_inner ret used  = function 
    | []                        ->  L.rev ret
    | h::t                      ->  assert (h > 0);assert (h <= 32); (* XXX using div and mod, generalization is possible *)
                                    arg_sizes_to_positions_inner(used+32-h::ret)(used+32)t

let arg_sizes_to_positions sizes = arg_sizes_to_positions_inner [] 4(*size of signature*) sizes

let pr_arg_loc r            = L.iter (fun(name,loc)->printf"arg %s at %s\n"name(as_string loc)) r

let args_with_locations (c:ty mthd) : (string*location)list = (match c.mthd_head with
    | Default           ->  []
    | Method h          ->  let sizes       = L.map calldata_size_of_arg h.mthd_args in
                                    let positions   = arg_sizes_to_positions sizes in
                                    let size_pos    = L.combine positions sizes in
                                    let locations   = L.map (fun(o,s)->(Calldata{calldata_offset=o;calldata_size=s}))size_pos in
                                    let names       = L.map (fun a -> a.id) h.mthd_args in
                                    let ret         = L.combine names locations in
                                    ret
  )

let get_array (raw:arg) : (string*ty*ty) option = match (raw.ty) with
    | TyMap(k, v)               ->  Some (raw.id, k, v)
    | _                         ->  None

let arrays_in_contract c : (string*ty*ty) list = 
    BL.filter_map get_array (c.contract_args)

let constructor_args (contract:ty contract) : (string*intf_ty) list = 
    get_intf_tys (contract.contract_args)

let total_size_of_intf_args lst : int = 
    try     BL.sum (L.map intf_ty_size lst) 
    with    Invalid_argument _          -> 0







module Hash = Cryptokit.Hash


let string_keccak str : string  =
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
        | [x]                       -> failwith "odd-length hex"
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

let keccak_signature (str:string) : string  =  String.sub (string_keccak str) 0 8

let string_of_intf_ty       = function 
    | IntfUint x                ->  "uint"  ^ (string_of_int x)
    | IntfBytes x               ->  "bytes" ^ (string_of_int x)
    | IntfAddr                  ->  "address"
    | IntfBool                  ->  "bool"

let string_of_mthd_info_ty (h:mthd_info) : string =
    let name_of_mthd    = h.mthd_name                           in
    let args            = get_intf_tys h.mthd_args         in
    let arg_tys         = L.map snd args                        in
    let str_tys         = L.map string_of_intf_ty arg_tys       in
    let ty              = String.concat "," str_tys             in
    name_of_mthd ^ "(" ^ ty ^ ")"

(* XXX: refactor with the above function *)
let string_of_event (e:event) : string =
    (* do I consider indexed no? *)
    let name            = e.event_name in
    let args       = get_intf_tys (L.map arg_of_event_arg e.event_args) in
    let arg_tys         = L.map snd args in
    let list_of_tys     = L.map string_of_intf_ty arg_tys in
    let args            = String.concat "," list_of_tys in
    name ^ "(" ^ args ^ ")"

let hash_of_mthd_info_ty (c:mthd_info) : string =
    let s               = string_of_mthd_info_ty c in
    keccak_signature s

let event_signature_hash (e:event) : string =
    let sign            = string_of_event e in
    keccak_signature sign

let compute_signature_hash (sign:string) : string =
    String.sub (string_keccak sign) 0 8

let hex_to_big_int h    = BB.big_int_of_string ("0x"^h)




