module PG where 


import Action 
import Asm 



toGC    :: RBTree OPCODE -> C
toGC    = undefined where 
    convert (RED JUMPI ts) = jumpi ts 
    jumpi  = undefined 


programGraph :: C -> [Action] 
programGraph = undefined 



