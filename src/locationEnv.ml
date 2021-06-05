(* le := location environment *) 


open Syntax 
open Imm
open ContractId
open Location 
open Misc

module L    = List
module Eth  = Ethereum


type locEnv             = (string * location) list 
type le                 = locEnv list

let err                 = failwith 
let size l              = BatList.sum (L.map L.length l)
let empty_env           = []


(** [update_locEnv [str] [new_loc] returns [None] when [str] is not found.
 *  Otherwise, it returns the updated block. *)
let update_locEnv key new_loc locEnv : locEnv option 
                            = err "update_locEnv"

let update le key new_loc   = filter_changeFst (update_locEnv key new_loc)le 

let lookup_loc key locEnv = 
    try   Some (L.assoc key locEnv)
    with  Not_found -> None

let lookup le key           = filter_getFst (lookup_loc key) le

let add_pair le (key,loc)   = match le with
    | []                      -> err "add_pair: no block"
    | h::t                    -> ((key,loc)::h)::t

let add_pairs le locEnv     = L.fold_left add_pair le locEnv

let add_empty_locEnv le      = [] :: le

let stack_story_locEnv locEnv : int option = err "stack_story_locEnv"

let last_stack_element_recorded le = match filter_getFst stack_story_locEnv le with
    | Some n                    ->  n
    | None                      -> -1

let cnstrctr_args_locations idx (args:(string*Eth.intf_ty)list) = 
    let total   = Eth.total_size_of_intf_args (L.map snd args) in
    let one_arg (name,offset,size) 
                = (name,Code{ code_start = Imm.(Minus(InitDataSize idx,(Int(total-offset))))
                            ; code_size  = Int size     }) in
    let rec name_offset_size_list rev_acc offset = function 
        (* match (args : (string*Eth.intf_ty)list) with *)  
        | []                    -> L.rev rev_acc
        | (h_name,h_ty)::t      -> name_offset_size_list ((h_name, offset, Eth.intf_ty_size h_ty) :: rev_acc)
                                    (offset + Eth.intf_ty_size h_ty) t in
    [L.map one_arg (name_offset_size_list [] 0 args)]

let cnstrctr_initial_env idx (cntrct : ty cntrct) =
    let args = Eth.cnstrctr_args cntrct in
    cnstrctr_args_locations idx args


(** [runtime_initial_t cntrct]
 * is a location environment that contains
 * the cnstrctr args
 * after StorConstrutorArgumentBegin *)
let runtime_initial_env (cntrct : ty cntrct) =
    let plain   = Eth.cnstrctr_args cntrct  in
    let init    = add_empty_locEnv empty_env in

    let f (le,word_idx) (nm,ty) =
        let size_in_word    = Eth.intf_ty_size ty / 32 in
        let loc             = Stor  { stor_start    = Int word_idx
                                    ; stor_size     = Int size_in_word } in
        let le'             = add_pair le (nm,loc) in
        le' , word_idx + size_in_word  in

    let le, mid = L.fold_left f (init,2) plain  in  (* XXX: remove the hard coded 2 *)
    let arrays  = Eth.arrays_in_cntrct cntrct   in  (* XXX: refactor the repetition *)

    let g (le,word_idx) (nm,_,_) =
        let size_in_word    = 1 in
        let loc             = Stor  { stor_start    = Int word_idx
                                    ; stor_size     = Int size_in_word } in
        let le'             = add_pair le (nm,loc) in
        le' , word_idx + size_in_word in

    let le, _   = L.fold_left g (le,mid) arrays in
    le
