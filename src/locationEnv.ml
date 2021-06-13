(* le := location environment *) 


open Misc
open Syntax 
open Imm
open IndexedList
open Location 

module L    = List
module BL   = BatList
module Eth  = Ethereum


type locEnv                         =   (string * location) list 
type le                             =   locEnv list

let size l                          =   BL.sum (L.map L.length l)
let emptyEnv                       =   []


let lookup_loc key locEnv           =   try   Some (L.assoc key locEnv)
                                        with  Not_found -> None

let lookup le key                   =   getFstFilter (lookup_loc key) le

let add_pair le (key,loc)           =   match le with
    | []                                -> err "add_pair: no block"
    | h::t                              -> ((key,loc)::h)::t

let add_pairs le locEnv             =   L.fold_left add_pair le locEnv

let add_mthd_argLocs mthd le        =
                                        let locEnv = Eth.argLocs_of_mthd mthd in
                                        add_pairs le locEnv

let add_empty_locEnv le     = [] :: le





let cnstrctr_args_locations idx (args:(string*ty)list) = 
    let total   = BL.sum (L.map size_of_ty (L.map snd args)) in
    let one_arg (name,offset,size) 
                = (name,Code{ code_start = Imm.(Minus(InitDataSize idx,(Int(total-offset))))
                            ; code_size  = Int size     }) in
    let rec name_offset_size_list rev_acc offset = function 
        | []                    -> L.rev rev_acc
        | (name,ty)::t      -> name_offset_size_list ((name, offset, size_of_ty ty) :: rev_acc)
                                    (offset + size_of_ty ty) t in
    [L.map one_arg (name_offset_size_list [] 0 args)]

let cnstrctr_initial_env idx (cntrct : ty cntrct) =
    let args = Eth.cnstrctr_args cntrct in
    cnstrctr_args_locations idx args


(** [rntime_initial_t cntrct]
 * is a location environment that contains
 * the cnstrctr args
 * after StorConstrutorArgumentBegin *)
let rntime_initial_env (cntrct : ty cntrct) =
    let plain               = Eth.cnstrctr_args cntrct  in
    let init                = add_empty_locEnv emptyEnv in

    let f (le,word_idx) (nm,ty) =
        let size_in_word    = size_of_ty ty / 32 in
        let loc             = Stor  { stor_start    = Int word_idx
                                    ; stor_size     = Int size_in_word } in
        let le'             = add_pair le (nm,loc) in
        le' , word_idx + size_in_word  in

    let le, mid             = L.fold_left f (init,2) plain  in  (* XXX: remove the hard coded 2 *)
    let arrays              = Eth.getArr_of_cntrct cntrct   in  (* XXX: refactor the repetition *)

    let g (le,word_idx) (nm,_,_) =
        let size_in_word    = 1 in
        let loc             = Stor  { stor_start    = Int word_idx
                                    ; stor_size     = Int size_in_word } in
        let le'             = add_pair le (nm,loc) in
        le' , word_idx + size_in_word in

    let le, _   = L.fold_left g (le,mid) arrays in
    le
