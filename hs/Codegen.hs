
module Codegen where 

import PreLink
import Opcode
import Hex 
import Utils

import Data.Word 
import Text.Printf(printf)
import Asm 
import Mem 

import Text.Read (readMaybe) 





----------------------------------------------
---             Malloc                     ---
----------------------------------------------



malloc1 :: [OPCODE] 
malloc1 = alloc1 _MP_ 

mfree1 :: [OPCODE] 
mfree1 = free1 _MP_ 

malloc :: Int -> [OPCODE] 
malloc n = [PUSH1 _MP_, MLOAD, DUP1, PUSH4(to n * 0x20), ADD, PUSH1 _MP_, MSTORE] 






------------------------------------------------
---   TRANSPILER   &   COMPILER              ---
------------------------------------------------


-- 1. Transpile pseudo code to real code 
--      a. FUNSTACK transpile
-- 2. set real values to pseudo values 
--      a. pushN operation decision  
--      b. code size dicision 
--      c. push operation decision with codesize 
-- 3. JUMPDEST/PUSHDEST decision 
-- 4. ADD CREATION CODE and make Layout 


codegen = codegen2 . codegen1 . codegen0 


-- 0. Header 

codegen0 :: [OPCODE] -> [OPCODE] 
codegen0 = header 


end0x03 :: [OPCODE] 
end0x03 = [PUSHDEST 0, JUMP,JUMPDEST 2, STOP, JUMPDEST 0] 

header :: [OPCODE] -> [OPCODE] 
header ops = end0x03 ++ init_malloc ++ init_mstack ++ ops 





-- 1. Transpiling 

codegen1  :: [OPCODE] -> [OPCODE] 
codegen1 = rmFUNSTACK

-- 1.a FUNSTACK transpile
rmFUNSTACKs :: [[OPCODE]] -> [[OPCODE]] 
rmFUNSTACKs = map rmFUNSTACK

rmFUNSTACK :: [OPCODE] -> [OPCODE] 
rmFUNSTACK []                       = []
rmFUNSTACK (PUSHFUNSTACK v : os)    = pushM v ++ rmFUNSTACK os
rmFUNSTACK (POPFUNSTACK    : os)    = popM1   ++ rmFUNSTACK os
rmFUNSTACK (o              : os)    = o       :  rmFUNSTACK os 






pushM v = [PUSH(FUN v)] ++ alloc1 _SP_ ++ [MSTORE] 
--pushM v = [PUSH(FUN v), PUSH1 _SP_, MLOAD, DUP1, PUSH1 _WORD_, ADD, PUSH1 _SP_, MSTORE, MSTORE] 
popM1   = free1 _SP_ ++ [MLOAD] 
--popM1   =                [PUSH1 _SP_, MLOAD, DUP1, PUSH1 _WORD_, SUB, PUSH1 _SP_, MSTORE, MLOAD] 
popM n  = [PUSH1 _SP_, MLOAD, DUP1, PUSH4 (to n * _WORD_), SUB, PUSH1 _SP_, MSTORE] ++ 
                concat ( replicate (n-1) [DUP1, MLOAD, PUSH1 _WORD_, SUB] ) ++ [MLOAD] 


-- 2
 
codegen2 :: [OPCODE] -> [OPCODE] 
codegen2 = rmPUSHs . map snd . rmPUSHDEST . reAddr2

-- 2.a size estimate  

withAddr :: [OPCODE] -> [(Int, OPCODE)] 
withAddr ops = zip (0 : scanl1 (+) (size <$> ops)) (ops ++ [INVALID])  

size_withAddr :: [(Int, OPCODE)] -> Int 
size_withAddr ops' = fst (last ops') + size (snd (last ops'))

size_RNTIME :: [OPCODE] -> Int 
size_RNTIME ops = size_withAddr (withAddr ops)

size_PUSHDEST :: [OPCODE] -> Int
size_PUSHDEST ops = 1 + (pushsize . to $ size_RNTIME ops)


reAddr :: [OPCODE] -> [(Int, OPCODE)] 
reAddr ops      =       loop ops [] where 
        sz = size_PUSHDEST ops
        loop [] ret                         = rev ret 
        loop (op:ops) ((i,PUSHDEST q):ios)  = loop ops ((i + sz, op):(i,PUSHDEST q):ios) 
        loop (op:ops) ((i,o         ):ios)  = loop ops ((i + size o, op):(i,o):ios)
        loop (op:ops) []                    = loop ops [(0,op)]

reAddr_sz :: Int -> [OPCODE] -> [(Int, OPCODE)] 
reAddr_sz sz ops      =       loop ops [] where 
        loop [] ret                         = rev ret 
        loop (op:ops) ((i,PUSHDEST q):ios)  = loop ops ((i + sz, op):(i,PUSHDEST q):ios) 
        loop (op:ops) ((i,o         ):ios)  = loop ops ((i + size o, op):(i,o):ios)
        loop (op:ops) []                    = loop ops [(0,op)]

reAddr2 :: [OPCODE] -> [(Int, OPCODE)] 
reAddr2 ops = reAddr_sz (1 + (pushsize $ to $ size_withAddr (reAddr ops))) ops





-- 2.b after size decision, we can extract jumpdest address

rmPUSHDEST :: [(Int, OPCODE)] -> [(Int, OPCODE)] 
rmPUSHDEST ops = loop ops where 
    sz = to $ size_withAddr ops
    loop []     = []
    loop ((i,PUSHDEST q):rest) = (i, mkPUSH (pushsize sz) (to $ findDEST q ops)): loop rest 
    loop (o:os) = o : loop os 

findDEST :: Int -> [(Int,OPCODE)] -> Int
findDEST i ops = try i ops where 
    try i []                           = findDEST 2 ops
    try i ((d,JUMPDEST i'):os) | i==i' = d 
    try i (o:os)                       = try i os 







-- 2.c remove `PUSH`  
rmPUSHs :: [OPCODE] -> [OPCODE] 
rmPUSHs = map rmPUSH

rmPUSH  :: OPCODE -> OPCODE 
rmPUSH (PUSH(FUN i)) | 0<=i && i<maxBound   =   mkPUSH(pushsize(to i))(to i)
rmPUSH (PUSH(INT i)) | 0<=i && i<2^255      =   mkPUSH(pushsize    i )    i 
rmPUSH (PUSH v)                             =   error $ "rmPUSH: undefined " ++ show v
rmPUSH o                                    =   o 

mkPUSH :: Int -> Integer -> OPCODE
mkPUSH _ 0 = PUSH0 
mkPUSH n x = debugRead ("PUSH" ++ show n ++ " " ++ show x) 

debugRead x = case readMaybe x of 
                Just y -> y 
                Nothing -> error $ "read error : cannot read " ++ show x 


{--
data PreLinkValue   = LABEL Int             -- JUMPDEST Label 
                    | FUN Int               -- Function Stack Number
                    | CR_SIZE               -- creation codesize
                    | RN_SIZE               -- runtime codesize 
                    | RN_OFFSET 
                    | LIB_ADDR              -- library address
                    | INT Integer 
                    deriving (Show, Eq, Read) 
--} 





-------------------------------------------
---     4. Creation Code               ---
-------------------------------------------

creationCode :: Int -> [OPCODE] 
creationCode q = init_malloc ++ init_mstack ++ snd (if_value_revert 0) ++ libcreate ++ deploy 


-- a. init memory 
init_malloc :: [OPCODE] 
init_malloc = [PUSH3 _MEM_, PUSH1 _MP_, MSTORE] 

-- b. init function stack 
init_mstack :: [OPCODE] 
init_mstack = [PUSH2 _STK_, PUSH1 _SP_, MSTORE] 

-- c. the guard code against value sending 
if_value_revert :: Int -> (Int, [OPCODE]) 
if_value_revert q = (q+1, opcodes) where 
    opcodes = [JUMPDEST q, CALLVALUE, ISZERO, PUSHDEST (q+1), JUMPI, PUSH0, PUSH0, REVERT]


-- #TODO 
-- d. library creation 
-- using 
-- (CREATE /) CREATE2 opcode, 
-- create library contracts in the creation code and, 
-- later, CALL them from runtime code. 

libcreate = [] -- undefined 


-- e. codecopy for constructor ( deploying contract ) 

-- push code size 
-- DUP1 
-- CODECOPY(to: 0x00, from: RUNTIME_CODE_OFFSET, size: ARG[1]) 
-- RETURN(0x00, ARG[1]) 

deploy :: [OPCODE] 
deploy =  [PUSH RN_SIZE, DUP1, PUSH RN_OFFSET, PUSH0, CODECOPY, PUSH0, RETURN] 









-- #TODO 
-- precalculate CREATION ADDRESS 
--  import RLP 
--  import CRYPTO (keccak256) 
--

