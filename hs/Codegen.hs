
module Codegen where 


import Opcode
import Hex 




init_malloc :: [OPCODE] 
init_malloc = [PUSH1 0x80, PUSH1 0x40, MSTORE] 

init_mstack :: [OPCODE] 
init_mstack = [PUSH2 0x3FFF, PUSH1 0x60, MSTORE] 

if_value_revert :: Int -> (Int, [OPCODE]) 
if_value_revert q = (q+1, opcodes) where 
    opcodes = [JUMPDEST q, CALLVALUE, ISZERO, PUSHDEST(q+1), JUMPI, PUSH0, PUSH0, REVERT]








