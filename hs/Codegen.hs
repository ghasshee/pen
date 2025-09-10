
module Codegen where 

import PreLink
import Opcode
import Hex 

import Data.Word 
import Text.Printf(printf)
import Asm 









----------------------------------------------
---             Malloc                     ---
----------------------------------------------



malloc1 :: [OPCODE] 
malloc1 = [PUSH1 0x40, MLOAD, DUP1, PUSH1 0x20, ADD, PUSH1 0x40, MSTORE]  

malloc :: Int -> [OPCODE] 
malloc n = [PUSH1 0x40, MLOAD, DUP1, PUSH4 (0x20 * toInteger n), ADD, PUSH1 0x40, MSTORE] 






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
removeFUNSTACKOPCODE :: [OPCODE] -> [OPCODE] 
removeFUNSTACKOPCODE []                       = []
removeFUNSTACKOPCODE (PUSHFUNSTACK v : os)    = pushM v ++ removeFUNSTACKOPCODE os
removeFUNSTACKOPCODE (POPFUNSTACK    : os)    = popM1   ++ removeFUNSTACKOPCODE os
removeFUNSTACKOPCODE (o              : os)    = o       :  removeFUNSTACKOPCODE os 

removeFUNSTACKOPCODEs :: [[OPCODE]] -> [[OPCODE]] 
removeFUNSTACKOPCODEs = map removeFUNSTACKOPCODE





pushM v = [PUSH (FUN v), PUSH1 0x60, MLOAD, DUP1, PUSH1 0x20, ADD, PUSH1 0x60, MSTORE, MSTORE] 
popM1   = [PUSH1 0x60, MLOAD, DUP1, PUSH1 0x20, SUB, PUSH1 0x60, MSTORE, MLOAD] 
popM n  = [PUSH1 0x60, MLOAD, DUP1, PUSH4 (0x20 * toInteger n), SUB, PUSH1 0x60, MSTORE] ++ 
                concat ( replicate (n-1) [DUP1, MLOAD, PUSH1 0x20, SUB] ) ++ [MLOAD] 




-- 2.a PUSH N decision 
pushN_decision :: OPCODE -> OPCODE 
pushN_decision (PUSH v) = case v of 
    FUN   i                 -> if 0 <= i && i < 2^64  then push_of_size (size_int i) (toInteger i) else error "pushN_decision: out of range (FUN i)" 
    INT   i                 -> if 0 <= i && i < 2^256 then push_of_size (size_integer i) i         else error "pushN_decision: out of range (INT i)"  
    _                       -> PUSH v 
pushN_decision o        = o 

pushN_desisions :: [OPCODE] -> [OPCODE] 
pushN_desisions = map pushN_decision



push_of_size :: Int -> (Integer -> OPCODE) 
push_of_size size = case size of 
    1               -> PUSH1 
    2               -> PUSH2
    3               -> PUSH3 
    4               -> PUSH4
    5               -> PUSH5
    6               -> PUSH6
    7               -> PUSH7
    8               -> PUSH8
    9               -> PUSH9
    10              -> PUSH10
    11              -> PUSH11
    12              -> PUSH12
    13              -> PUSH13
    14              -> PUSH14
    15              -> PUSH15
    16              -> PUSH16
    17              -> PUSH17
    18              -> PUSH18
    19              -> PUSH19
    20              -> PUSH20
    21              -> PUSH21
    22              -> PUSH22
    23              -> PUSH23
    24              -> PUSH24
    25              -> PUSH25
    26              -> PUSH26
    27              -> PUSH27
    28              -> PUSH28
    29              -> PUSH29
    30              -> PUSH30
    31              -> PUSH31
    32              -> PUSH32
    _               -> error "push_of_size : out of range (size N)" 



size_integer :: Integer -> Int 
size_integer i = if div16 == 0 then 1 else size_integer div16 + 1 
    where   div16 = i `div` 16 

size_int    :: Int      -> Int
size_int     i = if div16 == 0 then 1 else size_int div16 + 1
    where   div16 = i `div` 16 



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

creationCode :: [OPCODE] 
creationCode = init_malloc ++ init_mstack ++ snd (if_value_revert 0) ++ libcreate ++ deploy 

-- a. init memory 
init_malloc :: [OPCODE] 
init_malloc = [PUSH1 0x1000, PUSH1 0x40, MSTORE] 

-- b. init function stack 
init_mstack :: [OPCODE] 
init_mstack = [PUSH2 0x80, PUSH1 0x60, MSTORE] 

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

