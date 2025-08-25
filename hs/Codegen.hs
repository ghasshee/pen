
module Codegen where 

import PreLink
import Opcode
import Hex 

import Data.Word 
import Text.Printf(printf)



init_malloc :: [OPCODE] 
init_malloc = [PUSH1 0x1000, PUSH1 0x40, MSTORE] 

init_mstack :: [OPCODE] 
init_mstack = [PUSH2 0x80, PUSH1 0x60, MSTORE] 

if_value_revert :: Int -> (Int, [OPCODE]) 
if_value_revert q = (q+1, opcodes) where 
    opcodes = [JUMPDEST q, CALLVALUE, ISZERO, PUSH(LABEL(q+1)), JUMPI, PUSH0, PUSH0, REVERT]











malloc1 :: [OPCODE] 
malloc1 = [PUSH1 0x40, MLOAD, DUP1, PUSH1 0x20, ADD, PUSH1 0x40, MSTORE]  

malloc :: Int -> [OPCODE] 
malloc n = [PUSH1 0x40, MLOAD, DUP1, PUSH4 (0x20 * toInteger n), ADD, PUSH1 0x40, MSTORE] 


pushM v = [PUSH (FUN v), PUSH1 0x60, MLOAD, DUP1, PUSH1 0x20, ADD, PUSH1 0x60, MSTORE, MSTORE] 
popM1   = [PUSH1 0x60, MLOAD, DUP1, PUSH1 0x20, SUB, PUSH1 0x60, MSTORE, MLOAD] 
popM n  = [PUSH1 0x60, MLOAD, DUP1, PUSH4 (0x20 * toInteger n), SUB, PUSH1 0x60, MSTORE] ++ 
                concat ( replicate (n-1) [DUP1, MLOAD, PUSH1 0x20, SUB] ) ++ [MLOAD] 







removeFUNSTACKOPCODE :: [OPCODE] -> [OPCODE] 
removeFUNSTACKOPCODE []                       = []
removeFUNSTACKOPCODE (PUSHFUNSTACK v : os)    = pushM v ++ removeFUNSTACKOPCODE os
removeFUNSTACKOPCODE (POPFUNSTACK    : os)    = popM1   ++ removeFUNSTACKOPCODE os
removeFUNSTACKOPCODE (o              : os)    = o       :  removeFUNSTACKOPCODE os 

removeFUNSTACKOPCODEs :: [[OPCODE]] -> [[OPCODE]] 
removeFUNSTACKOPCODEs = map removeFUNSTACKOPCODE



