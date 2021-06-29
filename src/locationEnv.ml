open Big_int
open Misc
open Syntax 
open Abstract
open IndexedList

module L    = List
module BL   = BatList
module Eth  = Ethereum


(**********************************)
(*   LE := LOCATION ENVIRONMENTS  *) 
(**********************************)
type locEnv                         =   (string * location) list 
type le                             =   locEnv list

let size l                          =   BL.sum (L.map L.length l)
let empty_le                        =   []
let add_emptyEnv le                 =   [] :: le

let lookup_loc key locEnv           =   try   Some (L.assoc key locEnv)
                                        with  Not_found -> None
let lookup le key                   =   getFstFilter (lookup_loc key) le
let add_loc le (key,loc)            =   match le with
    | []                                -> err "add_loc: no block"
    | h::t                              -> ((key,loc)::h)::t
let add_locs le locs                =   L.fold_left add_loc le locs
let add_mthd_argLocs mthd le        =   add_locs le (Eth.argLocs_of_mthd mthd)


(** [rntime_initial_t cntrct]
 * is a location environment that contains
 * the cnstrctr args
 * after StorConstrutorArgumentBegin *)
let addArg(le,idx)(nm,ty)       =
    let size                = size_of_ty ty / 32                in
    let loc                 = Stor  { stor_start = Int idx
                                    ; stor_size  = Int size }   in
    let le'                 = add_loc le (nm,loc)              in
    le' , idx + size  
let addArr(le,idx)(nm,_,_)      =
    let size                = 1                                 in
    let loc                 = Stor { stor_start = Int idx
                               ; stor_size  = Int size }    in
    let le'                 = add_loc le (nm,loc)              in
    le' , idx + size                                        
let rntime_init_le (cn:ty cntrct) =
    let argTys              = Eth.argTys_of_cntrct cn           in
    let init                = add_emptyEnv empty_le             in
    let le, mid             = L.fold_left addArg (init,2)argTys in  
    let arrays              = Eth.getArr_of_cntrct cn           in  
    let le, _   = L.fold_left addArr (le,mid) arrays            in
    le

