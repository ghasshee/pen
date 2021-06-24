open Misc


type effector   = Stor | External (*| Balance*)
type rw         = Read | Write
type eff        = effector * rw 

let isRead  eff     = snd eff=Read 
let isWrite eff     = snd eff=Write
let isStorRead  eff = eff=(Stor,Read) 
let isStorWrite eff = eff=(Stor,Write)
let atStor  eff     = fst eff=Stor
let atExt   eff     = fst eff=External
