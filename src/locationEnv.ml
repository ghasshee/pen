(* le := location environment *) 


open Syntax 
open Imm
open ContractId
open Location 
open Misc


type le                 = (string * location) list list

let size l              = BatList.sum (List.map List.length l)
let empty_env           = []
let forget_innermost    = function
  | (_::older)              -> older
  | []                      -> failwith "forget_innermost: no blocks to forget"

(** [update_block [str] [new_loc] returns [None] when [str] is not found.
 *  Otherwise, it returns the updated block. *)
let update_block   (key:string)(new_loc:location)(blk:(string*location)list) :
      (string * location) list option = failwith "update_block"

let update  (le:le)(key:string)(new_loc:location) = filter_changeFst(update_block key new_loc)le 

let lookup_block (key:string)(l:(string*location)list) = 
    try   Some (List.assoc key l)
    with  Not_found -> None

let lookup  (le:le)(key:string)                 = filter_getFst (lookup_block key) le

let add_pair(le:le)(key:string)(loc:location)   = match le with
    | []                      -> failwith "add_pair: no block"
    | h::t                    -> ((key,loc)::h)::t

let add_pairs (le:le) (l:(string*location)list) =
    List.fold_left (fun le'(key,loc) -> add_pair le' key loc) le l

let add_empty_block le                          = [] :: le

let stack_story_block (blk:(string*location)list) : int option = failwith "stack_story_block"

let last_stack_element_recorded (le:le) = match filter_getFst stack_story_block le with
    | Some n                    ->  n
    | None                      -> -1

let cnstrctr_args_locations (cid:cid) (args:(string*Ethereum.intf_ty)list) = 
    let total   = Ethereum.total_size_of_intf_args (List.map snd args) in
    let one_arg((name:string),(offset:int),(size:int)) 
                = (name, Code   { code_start = Imm.(Minus(InitDataSize cid,(Int(total-offset))))
                                ; code_size  = Int size     }) in
    let rec name_offset_size_list rev_acc offset = function 
        (* match (args : (string*Ethereum.intf_ty)list) with *)  
        | []                    -> List.rev rev_acc
        | (h_name,h_ty)::t      -> name_offset_size_list ((h_name, offset, Ethereum.intf_ty_size h_ty) :: rev_acc)
                                    (offset + Ethereum.intf_ty_size h_ty) t in
    [List.map one_arg (name_offset_size_list [] 0 args)]

let cnstrctr_initial_env (cid : cid) (cntrct : ty cntrct) =
    let args = Ethereum.cnstrctr_args cntrct in
    cnstrctr_args_locations cid args


(** [runtime_initial_t cntrct]
 * is a location environment that contains
 * the cnstrctr args
 * after StorConstrutorArgumentBegin *)
let runtime_initial_env (cntrct : ty cntrct) =
  let plain = Ethereum.cnstrctr_args cntrct in
  let init = add_empty_block empty_env in
  let f (lenv, word_idx) (name, typ) =
    let size_in_word = Ethereum.intf_ty_size typ / 32 in
    let loc = (Stor {
                  stor_start = Int word_idx;
                  stor_size = Int size_in_word
                }) in
    let new_lenv = add_pair lenv name loc in
    (new_lenv, word_idx + size_in_word)
  in
  (* XXX: remove the hard coded 2 *)
  let (le, mid) = List.fold_left f (init, 2) plain in
  let arrays = Ethereum.arrays_in_cntrct cntrct in (* XXX: refactor the repetition *)
  let g (lenv, word_idx) (name, _, _) =
    let size_in_word = 1 in
    let loc = (Stor {
                            stor_start = Int word_idx;
                            stor_size = Int size_in_word
              }) in
    let new_lenv = add_pair lenv name loc in
    (new_lenv, word_idx + size_in_word) in
  let (le, _) = List.fold_left g (le, mid) arrays in
  le
