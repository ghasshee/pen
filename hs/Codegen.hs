
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


codegen = codegen4 . codegen3 . codegen2 . codegen1 . codegen0 


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




-- 2.  Addressing (Determine JUMP Addresses) 
 
codegen2 :: [OPCODE] -> PROGRAM
codegen2 = rmPUSHs . rmPUSHDEST . addressing

-- 2.a size estimate  

type PROGRAM = [(Int, OPCODE)] 

mkPROGRAM :: [OPCODE] -> PROGRAM
mkPROGRAM ops = zip (0 : scanl1 (+) (size <$> ops)) (ops ++ [INVALID])  

sizePROGRAM :: PROGRAM -> Int 
sizePROGRAM prog = fst (last prog) + size (snd (last prog))

reAddrPROGRAM :: Int -> [OPCODE] -> PROGRAM  
reAddrPROGRAM sz ops      =       loop ops [] where 
        pushsz = pushsize $ to sz
        loop [] ret                         = rev ret 
        loop (op:ops) ((i,PUSHDEST q):ios)  = loop ops ((i + pushsz, op):(i,PUSHDEST q):ios) 
        loop (op:ops) ((i,PUSH RN_OFFSET):ios)  = loop ops ((i + pushsz, op):(i,PUSH RN_OFFSET):ios) 
        loop (op:ops) ((i,o         ):ios)  = loop ops ((i + size o, op):(i,o):ios)
        loop (op:ops) []                    = loop ops [(0,op)]

addressing :: [OPCODE] -> PROGRAM
addressing ops = 
    let prog    = mkPROGRAM ops 
        prog'   = reAddrPROGRAM (sizePROGRAM prog ) ops 
        prog''  = reAddrPROGRAM (sizePROGRAM prog') ops in 
        prog''



-- 2.b after size decision, we can extract jumpdest address

rmPUSHDEST :: PROGRAM -> PROGRAM
rmPUSHDEST prog = loop prog where 
    n = pushdatasize (to (sizePROGRAM prog)) 
    loop []     = []
    loop ((i,PUSHDEST q):rest) = (i, mkPUSH n (to $ findDEST q prog)): loop rest 
    loop (o:os) = o : loop os 

findDEST :: Int -> PROGRAM -> Int
findDEST i ops = try i ops where 
    try i []                           = findDEST 2 ops
    try i ((d,JUMPDEST i'):os) | i==i' = d 
    try i (o:os)                       = try i os 



-- 2.c remove `PUSH`  
rmPUSHs :: PROGRAM -> PROGRAM 
rmPUSHs = map (fmap rmPUSH)

rmPUSH  :: OPCODE -> OPCODE 
rmPUSH (PUSH(FUN i)) | 0<=i && i<maxBound   =   mkPUSH(pushdatasize(to i))(to i)
rmPUSH (PUSH(INT i)) | 0<=i && i<2^255      =   mkPUSH(pushdatasize    i )    i 
rmPUSH (PUSH v)                             =   error $ "rmPUSH: undefined " ++ show v
rmPUSH o                                    =   o 

mkPUSH :: Int -> Integer -> OPCODE
mkPUSH _ 0 = PUSH0 
mkPUSH n x = read_DEBUG ("PUSH" ++ show n ++ " " ++ show x) 

read_DEBUG x = case readMaybe x of 
                Just y -> y 
                Nothing -> error $ "read error : cannot read " ++ show x 



-------------------------------------------
---     3.                              ---
-------------------------------------------

codegen3 = id 



-------------------------------------------
---     4. Creation Code               ---
-------------------------------------------

codegen4 :: PROGRAM -> [OPCODE] 
codegen4 = map snd . addCreationCode 



creationCode :: [OPCODE] 
creationCode = init_malloc ++ init_mstack ++ snd (if_value_revert 0) ++ libcreate ++ deploy ++ [INVALID] 

addCreationCode :: PROGRAM -> PROGRAM 
addCreationCode prog = 
    let rn_size     = sizePROGRAM prog 
        cr_code     = creationCode
        cr_prog     = mkPROGRAM cr_code
        cr_size     = sizePROGRAM cr_prog
        cr_prog'    = reAddrPROGRAM cr_size cr_code
        cr_size'    = sizePROGRAM cr_prog' 
        cr_prog''   = reAddrPROGRAM cr_size' cr_code
        cr_prog'''  = layout rn_size cr_size' cr_prog'' 
        cr_prog'''' = rmPUSHDEST cr_prog''' in 
        cr_prog'''' ++ prog
        


    


-- a. init memory 
init_malloc :: [OPCODE] 
init_malloc = [PUSH3 _MEM_, PUSH1 _MP_, MSTORE] 

-- b. init function stack 
init_mstack :: [OPCODE] 
init_mstack = [PUSH2 _STK_, PUSH1 _SP_, MSTORE] 

-- c. the guard code against value sending 
if_value_revert :: Int -> (Int, [OPCODE]) 
if_value_revert q = (q+1, opcodes) where 
    opcodes = [CALLVALUE, ISZERO, PUSHDEST q, JUMPI, PUSH0, PUSH0, REVERT, JUMPDEST q] 


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
deploy =  [PUSH RN_SIZE, PUSH0, PUSH RN_OFFSET, DUP3, CODECOPY, PUSH0, RETURN] 


layout :: Int -> Int -> PROGRAM -> PROGRAM 
layout rn_size rn_offst prog = map (rmRN_INFO rn_size rn_offst <$>) prog

rmRN_INFO :: Int -> Int -> OPCODE -> OPCODE
rmRN_INFO sz offst (PUSH RN_SIZE)   = mkPUSH (pushdatasize (to sz))    (to sz) 
rmRN_INFO sz offst (PUSH RN_OFFSET) = mkPUSH (pushdatasize (to offst)) (to offst) 
rmRN_INFO sz offst o                = o 






-- #TODO 
-- precalculate CREATION ADDRESS 
--  import RLP 
--  import CRYPTO (keccak256) 
--
