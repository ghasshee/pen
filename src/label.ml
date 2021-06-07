
(* label *) 

open Printf 

type label                      =   int

let current_label               =   ref 0 
let fresh_label ()              =   let l          = !current_label in 
                                    current_label := !current_label+1; 
                                    l

let labeltbl:(label*int)list ref=   ref []

let register_label label line   =   labeltbl := (label,line) :: !labeltbl  
let lookup_label l              =   List.assoc l !labeltbl
                                    













(* entrypoint Database *) 


open IndexedList

type entrypoint =
                | Cntrct  of idx
                | Mthd    of idx * Syntax.mthd_head

let store : (entrypoint*label)list ref          = ref []
let register_entrypoint(k:entrypoint)(v:label)  = store := (k, v) :: !store
let lookup_entrypoint (k : entrypoint) : label  = List.assoc k !store












