open Misc


type eff_loc    = Stor | External (*| Balance*)
type rw         = Read | Write
type eff        = eff_loc * rw 

let isRead  eff     = snd eff=Read 
let isWrite eff     = snd eff=Write
let isStorRead  eff = eff=(Stor,Read) 
let isStorWrite eff = eff=(Stor,Write)
let atStor  eff     = fst eff=Stor
let atExt   eff     = fst eff=External
