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
let empty_le                        = []
let add_emptyEnv le                 = [] :: le

let lookup_loc key locEnv           =   try   Some (L.assoc key locEnv)
                                        with  Not_found -> None
let lookup le key                   =   getFstFilter (lookup_loc key) le
let add_pair le (key,loc)           =   match le with
    | []                                -> err "add_pair: no block"
    | h::t                              -> ((key,loc)::h)::t
let add_pairs le pairs              =   L.fold_left add_pair le pairs
let add_mthd_argLocs mthd le        =   add_pairs le (Eth.argLocs_of_mthd mthd)




(** [rntime_initial_t cntrct]
 * is a location environment that contains
 * the cnstrctr args
 * after StorConstrutorArgumentBegin *)
let rntime_initEnv (cn:ty cntrct) =
    let plain               = Eth.argTys_of_cntrct cn   in
    let init                = add_emptyEnv empty_le     in
    let f (le,idx) (nm,ty) =
        let size            = size_of_ty ty / 32        in
        let loc             = Stor { stor_start = Int idx
                                   ; stor_size  = Int size } in
        let le'             = add_pair le (nm,loc) in
        le' , idx + size  in

    let le, mid             = L.fold_left f (init,2) plain  in  (* XXX: remove the hard coded 2 *)
    let arrays              = Eth.getArr_of_cntrct cn       in  (* XXX: refactor the repetition *)

    let g (le,word_idx) (nm,_,_) =
        let size_in_word    = 1 in
        let loc             = Stor { stor_start = Int word_idx
                                   ; stor_size  = Int size_in_word } in
        let le'             = add_pair le (nm,loc) in
        le' , word_idx + size_in_word in

    let le, _   = L.fold_left g (le,mid) arrays in
    le
