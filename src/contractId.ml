
open Printf 
open Misc

module L    = List
module BL   = BatList 
module BO   = BatOption 

type idx                =   int
type 'a indexed_list        =   (idx * 'a) list


let fold_indexed_list       =   function 
    | []                    -> [] 
    | l                     -> L.combine BL.(range 0 `To (L.length l - 1)) l  
let map              f  =   L.map (fun(id,x)->id,f x) 
let pair_map         f  =   L.map (fun(id,x)->id,f id x) 
let filter_map       f  =   BL.filter_map (fun(id,x)-> BO.map (fun ret->id,ret) (f x))
let choose_cntrct    l  =   try   L.assoc l 
                            with e-> eprintf "choose_cntrct: ";raise e
let pr_idx_mapping   f  =   L.iter (fun idx->printf "%d ↦ %d, " idx (f idx))
let insert id a l       =   (id,a)::l       (* shall I sort it?  Maybe later at once. *)
let lookup_id f l       =   let id,_ = L.find (f $ snd) l in id 
let empty               =   []
let idxs     l          =   L.map fst l
let values   l          =   L.map snd l
