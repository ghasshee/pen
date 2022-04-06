module Link where 

import Tree
import Asm 
import Hex 






destJUMPI :: [RBTree OPCODE] -> String 
destJUMPI xs = loop xs [] True where 
    loop [] [a,b] True                      = a 
    loop [] [a,b] False                     = b 
    loop [] _     _                         = ""
    loop (RED (PUSH1 a)as: xs) ret swap     = loop xs (a:ret) swap
    loop (RED (PUSH2 a)as: xs) ret swap     = loop xs (a:ret) swap
    loop (RED (PUSH3 a)as: xs) ret swap     = loop xs (a:ret) swap
    loop (RED (PUSH4 a)as: xs) ret swap     = loop xs (a:ret) swap
    loop (RED o        as: xs) ret swap     = loop xs (show o:ret) swap
    loop (BLK SWAP1 _:xs) ret swap          = loop xs ret     (not swap) 
    loop (x:xs) ret swap                    = loop xs ret     swap

destJUMP :: [RBTree OPCODE] -> String 
destJUMP xs     = loop xs [] where 
    loop [] [a]                             = a 
    loop (RED (PUSH1 a)as: xs) ret          = loop xs (a:ret) 
    loop (RED (PUSH2 a)as: xs) ret          = loop xs (a:ret) 
    loop (RED o        as: xs) ret          = loop xs ("FFFFFFFFFFFFFF":ret) 
    loop (BLK _ _        : xs) ret          = loop xs ret 


assocDEST :: String -> [RBTree OPCODE] -> RBTree OPCODE
assocDEST dest []     = BLK (UNDEFINED "NO LINK") [] 
assocDEST dest (x:xs) = if fromHex dest == 0 then BLK (UNDEFINED "RESTART") [] else 
    case x of 
    BLK SEQ (BLK (JUMPDEST s)_:_) -> if fromHex s == fromHex dest 
                                        then x 
                                        else assocDEST dest xs 
    _                             -> assocDEST dest xs 

link :: [RBTree OPCODE] -> [RBTree OPCODE] 
link trees = ln trees trees where 
    ln []                  whole    =   [] 
    ln (BLK JUMPI as : xs) whole    =   let dest = destJUMPI as in 
                                        let tree = [assocDEST dest whole] in 
                                        let cont = ln tree whole in 
                                        BLK JUMPI (ln as whole ++ cont) : ln xs whole
    ln (BLK JUMP as  : xs) whole    =   let dest = destJUMP as in 
                                        let tree = [assocDEST dest whole] in 
                                        let cont = ln tree whole in 
                                        BLK JUMP (ln as whole ++ cont) : ln xs whole 
    ln (BLK a as : xs)     whole    =   BLK a (ln as whole) : ln xs whole
    ln (RED a as : xs)     whole    =   RED a (ln as whole) : ln xs whole

