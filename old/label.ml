open Printf 
open Misc

(*********************)
(***    LABEL      ***)
(*********************)

type label                      =   int
type line                       =   int 
type labelTbl                   =   (label * line) list ref 

let labelTbl: labelTbl          =   ref []
let current_label               =   ref 0 
let fresh_label ()              =   let l          = !current_label in 
                                    current_label := !current_label+1; 
                                    l

let register_label label line   =   labelTbl := (label,line) :: !labelTbl  
let lookup_label   label        =   List.assoc label !labelTbl 
                                    

(**********************)
(***    ENTRY       ***)
(**********************)

type entry                      =   Cntrct  of idx
                                |   Mthd    of idx * Syntax.ty
type entryTbl                   =   (entry * label) list ref 

let entryTbl : entryTbl         = ref []
let register_entry k v          = entryTbl := (k,v)::!entryTbl
let lookup_entry k              = List.assoc k !entryTbl


(**********************)
(***    LAMBDA      ***)
(**********************)

type lambda_idx                 =   int

let lambda_idx                  =   ref 0 
let fresh_idx ()                =   let i = !lambda_idx in    
                                    lambda_idx := i+1; 
                                    i 










