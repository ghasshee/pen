
open Printf 

type label                      = int

let fresh_label                 = ref 0 
let store :(label*int)list ref  = ref []

let get_new_label ()            = 
    let ret = !fresh_label in 
    fresh_label := !fresh_label + 1; 
    ret 

let register_value l i          =   store := (l,i) :: !store  

let lookup_value l              =   List.assoc l !store
                                    



(*
let debug_label                 = false

(* internal data not accessible from outside of the module. *)
let next_fresh_label            = ref 0
let store : (label*int)list ref = ref []

let new_label ()                =
  let ret                           = !next_fresh_label in
  if debug_label 
    then printf "label: generating label %d\n" ret 
    else ();
  next_fresh_label := ret + 1;
  ret

let register_value l i  =
    if debug_label then printf "label: registering label %d %d\n%!" l i; 
    store := (l, i) :: !store

let lookup_value l      =
  try       List.assoc l !store
  with Not_found -> if debug_label then eprintf "label: %d not found\n%!" l; raise Not_found
*)
