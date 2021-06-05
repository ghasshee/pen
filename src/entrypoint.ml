(* entrypoint Database *) 

open ContractId
open Label

type entrypoint =
                | Cntrct    of idx
                | Case        of idx * Syntax.mthd_head

let store : (entrypoint*label)list ref          = ref []
let register_entrypoint(k:entrypoint)(v:label)  = store := (k, v) :: !store
let lookup_entrypoint (k : entrypoint) : label  = List.assoc k !store
