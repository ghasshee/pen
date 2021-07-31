open Big_int
open Misc
open Syntax 
open Location
open TypeEnv
open IndexList

module L    = List
module BL   = BatList


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
let lookup le key                   =   getFstByFilter (lookup_loc key) le
let add_loc le (key,loc)            =   match le with
    | []                                -> err "add_loc: no block"
    | h::t                              -> ((key,loc)::h)::t
let add_locs le locs                =   foldl add_loc le locs
let add_mthd_argLocs mthd le        =   add_locs le (argLocs_of_mthd mthd)

let addArg(le,idx)(nm,ty)       =
    let size                = size_of_ty ty                 in
    let size                = if 0<size && size<=32 then 1 else size / 32  in
    let () = Printf.printf "%s has size %d and %d devided by 32\n"(string_of_ty ty) (size_of_ty ty) size in 
    let loc                 = Stor  { stor_start = Int idx
                                    ; stor_size  = Int size }   in
    let le'                 = add_loc le (nm,loc)               in
    le' , idx + size  
let addArr(le,idx)(nm,_,_)      =
    let size                = 1                                 in
    let loc                 = Stor  { stor_start = Int idx
                                    ; stor_size  = Int size }   in
    let le'                 = add_loc le (nm,loc)               in
    le' , idx + size                                        

(** [rntime_init_le]
 * is a location environment that contains 
 * the cnstrctr args after StorConstrutorArgumentBegin *)
let rntime_init_le (cn:ty cntrct) =
    let argTys              = argTys_of_cntrct cn               in
    let arrTys              = arrTys_of_cntrct cn               in  
    let init                = add_emptyEnv empty_le             in
    let le, mid             = foldl addArg (init,2) argTys      in  
    let le, _               = foldl addArr (le,mid) arrTys      in
    le

