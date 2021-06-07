
open Printf 
open Misc

module L    = List
module BL   = BatList 
module BO   = BatOption 

type idx                =   int
type 'a idx_list        =   (idx * 'a) list


let to_idx_list       =   function 
    | []                    -> [] 
    | l                     -> L.combine BL.(range 0 `To (L.length l - 1)) l  

let map              f  =   L.map (fun(i,x)->i,f x) 
let pair_map         f  =   L.map (fun(i,x)->i,f i x) 
let filter_map       f  =   BL.filter_map (fun(i,x)->BO.map(fun y->i,y)(f x))
let lookup_index    l  =   try   L.assoc l  with e-> eprintf "lookup_index: ";raise e
let pr_idx_mapping   f  =   L.iter (fun i->printf "%d ↦ %d, "i(f i))
let insert i a l        =   (i,a)::l       (* shall I sort it?  Maybe later at once. *)
let lookup_idx f l      =   let i,_ = L.find (f $ snd) l in i 
let empty               =   []
let idxs     l          =   L.map fst l
let values   l          =   L.map snd l


