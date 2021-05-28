(* cid := cntrct ID *) 

open Printf 
open Misc

module L    = List
module BL   = BatList 
module BO   = BatOption 

type cid                =   int
type 'a with_cid        =   (cid * 'a) list


let fold_with_cid       =   function 
    | []                    -> [] 
    | l                     -> L.combine BL.(range 0 `To (L.length l - 1)) l  
let map              f  =   L.map (fun(id,x)->(id,f x)) 
let pair_map         f  =   L.map (fun(id,x)->(id,f id x)) 
let filter_map       f  =   BL.filter_map (fun(id,x)-> BO.map (fun ret->(id,ret)) (f x))
let choose_cntrct  l  =   try   L.assoc l 
                            with  Not_found -> eprintf "choose_cntrct: not_found\n%!"; raise Not_found
let pr_cid_mapping   f  =   L.iter (fun cid -> printf "%d ↦ %d, " cid (f cid))
let insert id a orig    =   (id, a)::orig       (* shall I sort it?  Maybe later at once. *)
let lookup_id f l       =   let (id, _) = L.find (f $ snd) l in id 
let empty               =   []
let cids     l          =   L.map fst l
let values   l          =   L.map snd l
