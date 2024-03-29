module Eval where


import Asm 
import Tree
import Hex

absPUSH :: RBTree OPCODE -> RBTree OPCODE
absPUSH = fmap push where 
    push :: OPCODE -> OPCODE 
    push (PUSH1  s) = PUSH  s 
    push (PUSH2  s) = PUSH  s 
    push (PUSH3  s) = PUSH  s 
    push (PUSH4  s) = PUSH  s 
    push (PUSH5  s) = PUSH  s 
    push (PUSH6  s) = PUSH  s 
    push (PUSH7  s) = PUSH  s 
    push (PUSH8  s) = PUSH  s 
    push (PUSH9  s) = PUSH  s 
    push (PUSH10 s) = PUSH  s 
    push (PUSH11 s) = PUSH  s 
    push (PUSH12 s) = PUSH  s 
    push (PUSH13 s) = PUSH  s 
    push (PUSH14 s) = PUSH  s 
    push (PUSH15 s) = PUSH  s 
    push (PUSH16 s) = PUSH  s 
    push (PUSH17 s) = PUSH  s 
    push (PUSH18 s) = PUSH  s 
    push (PUSH19 s) = PUSH  s 
    push (PUSH20 s) = PUSH  s 
    push (PUSH21 s) = PUSH  s 
    push (PUSH22 s) = PUSH  s 
    push (PUSH23 s) = PUSH  s 
    push (PUSH24 s) = PUSH  s 
    push (PUSH25 s) = PUSH  s 
    push (PUSH26 s) = PUSH  s 
    push (PUSH27 s) = PUSH  s 
    push (PUSH28 s) = PUSH  s 
    push (PUSH29 s) = PUSH  s 
    push (PUSH30 s) = PUSH  s 
    push (PUSH31 s) = PUSH  s 
    push (PUSH32 s) = PUSH  s 
    push o          = o 


eval :: [RBTree OPCODE] -> [RBTree OPCODE] 
eval = fmap eval1 

eval1 :: RBTree OPCODE -> RBTree OPCODE 
eval1 = eval1T



eval1T (RED o os)           = RED o (eval1F os) 
eval1T (BLK o os)           = BLK o (eval1F os) 
eval1F []                   = []
eval1F (RED ADD os : ts)    = add (eval1F os) 0 ++ eval1F ts 
eval1F (t : ts)             = eval1T t  : eval1F ts 



add []                  n = [RED (PUSH (toHex n)) []] 
add (RED(PUSH m)_ : ts) n = add ts (n + fromHex m)
add (RED o os     : ts) n = RED o (eval os) : add ts n  
add (BLK o os     : ts) n = BLK o (eval os) : add ts n  

sub [] [n] = [RED (PUSH (toHex n)) []] 
sub (RED(PUSH n)_ : ts) []  = sub ts [fromHex n]
sub (RED(PUSH m)_ : ts) [n] = sub ts [n - fromHex m]
sub (RED o os     : ts) n = RED o os : sub ts n  
sub (BLK o os     : ts) n = BLK o os : sub ts n  
