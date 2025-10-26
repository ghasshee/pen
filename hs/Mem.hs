module Mem where 


import Opcode

-- Mem Types
_MP_    = 0x40 
_SP_    = 0x60

-- Mem Start Points  
_STK_   = 0x100 
_MEM_   = 0x10000


-- 1 word == 0x20 bytes == 256 bits  
_WORD_  = 0x20 


-- allocate & free 
alloc1 _MemType_ = [PUSH1 _MemType_, MLOAD, DUP1, PUSH1 _WORD_, ADD, PUSH1 _MemType_, MSTORE] 
free1  _MemType_ = [PUSH1 _MemType_, MLOAD, DUP1, PUSH1 _WORD_, SUB, PUSH1 _MemType_, MSTORE] 






--           ADDR    MEMORY 
--         +-------+----------------+
--         |0x00   |                | 
--         +-------+----------------+
--         |       |                | 
--         +-------+----------------+
--         |0x40   | 0x10000 MP     | 
--         +-------+----------------+
--         |0x60   | 0x100   SP     | 
--         +-------+----------------+
--         |       |                | 
--         +-------+----------------+
--         |0x100  | STACK          | 
--         +-------+----------------+
--         |       |                | 
--         +-------+----------------+
--         |0x10000| FREE MEM       | 
--         +-------+----------------+
--         |       |                | 
--         +-------+----------------+
--         |0xFFFF_FFFF_FFFF_FFFF == 2 ^ 64 |                | 
--         +-------+----------------+
--
--
--
--
--
--
--
--
--
--





