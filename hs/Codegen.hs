
module Codegen where 

import PreLink
import Opcode
import Hex 
import Utils

import Data.Word 
import Text.Printf(printf)
import Asm 
import Mem 






----------------------------------------------
---             Malloc                     ---
----------------------------------------------



malloc1 :: [OPCODE] 
malloc1 = [PUSH1 _MP_, MLOAD, DUP1, PUSH1 0x20, ADD, PUSH1 _MP_, MSTORE]  

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










-- 1. Transpiling 
-- 1.a FUNSTACK transpile
rmFUNSTACKs :: [[OPCODE]] -> [[OPCODE]] 
rmFUNSTACKs = map rmFUNSTACK

rmFUNSTACK :: [OPCODE] -> [OPCODE] 
rmFUNSTACK []                       = []
rmFUNSTACK (PUSHFUNSTACK v : os)    = pushM v ++ rmFUNSTACK os
rmFUNSTACK (POPFUNSTACK    : os)    = popM1   ++ rmFUNSTACK os
rmFUNSTACK (o              : os)    = o       :  rmFUNSTACK os 





pushM v = [PUSH8(to v), PUSH1 0x60, MLOAD, DUP1, PUSH1 0x20, ADD, PUSH1 0x60, MSTORE, MSTORE] 
popM1   = [PUSH1 0x60, MLOAD, DUP1, PUSH1 0x20, SUB, PUSH1 0x60, MSTORE, MLOAD] 
popM n  = [PUSH1 0x60, MLOAD, DUP1, PUSH4 (0x20 * to n), SUB, PUSH1 0x60, MSTORE] ++ 
                concat ( replicate (n-1) [DUP1, MLOAD, PUSH1 0x20, SUB] ) ++ [MLOAD] 


-- 2
 
codegen2 = rmPUSHs . map snd . rmPUSHDEST . reAddr

-- 2.a size estimate  

withAddr :: [OPCODE] -> [(Int, OPCODE)] 
withAddr ops = zip (0 : scanl1 (+) (size <$> ops)) (ops ++ [INVALID])  

sizeOPCODE :: [OPCODE] -> Int 
sizeOPCODE ops = 1 + fst ( last (withAddr ops) )  


pushdest_size ops = pushsize (to $ sizeOPCODE ops) 


reAddr :: [OPCODE] -> [(Int, OPCODE)] 
reAddr ops  =       
    loop ops [] where 
        sz = pushdest_size ops 
        loop [] ret                         = rev ret 
        loop (op:ops) ((i,PUSHDEST q):ios)  = loop ops ((i + sz, op):(i,PUSHDEST q):ios) 
        loop (op:ops) ((i,o         ):ios)  = loop ops ((i + size o, op):(i,o):ios)






-- 2.b after size decision, we can extract jumpdest address

rmPUSHDEST :: [(Int, OPCODE)] -> [(Int, OPCODE)] 
rmPUSHDEST ops = loop ops where 
    sz = 1 + fst (last ops) 
    loop []     = []
    loop ((i,PUSHDEST q):rest) = (i, mkPUSH sz (to $ findDEST q ops)): loop rest 
    loop (o:os) = o : loop os 

findDEST :: Int -> [(Int,OPCODE)] -> Int
findDEST i []                           = -1
findDEST i ((d,JUMPDEST i'):os) | i==i' = d 
findDEST i (o:os)                       = findDEST i os 







-- 2.c remove `PUSH`  
rmPUSHs :: [OPCODE] -> [OPCODE] 
rmPUSHs = map rmPUSH

rmPUSH  :: OPCODE -> OPCODE 
rmPUSH (PUSH(FUN i)) | 0<=i && i<2^64   =   mkPUSH(pushsize(to i))(to i)
rmPUSH (PUSH(INT i)) | 0<=i && i<2^256  =   mkPUSH(pushsize    i )    i 
rmPUSH (PUSH v)                         =   error $ "rmPUSH: undefined " ++ show v
rmPUSH o                                =   o 

mkPUSH :: Int -> Integer -> OPCODE
mkPUSH n x = read ("PUSH" ++ show n ++ " " ++ show x) 




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

