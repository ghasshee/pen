
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
                                    













(* entry Database *) 


open IndexList

type entry =
                | Cntrct  of idx
                | Mthd    of idx * Syntax.mthd_head

let entryTbl : (entry*label)list ref          = ref []
let register_entry(k:entry)(v:label)  = entryTbl := (k,v)::!entryTbl
let lookup_entry (k:entry)   = List.assoc k !entryTbl












