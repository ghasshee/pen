module Branch2Opcode where 

import Action
import Action2Opcode
import Branch 
import Opcode 



branches2opcodes :: [Branch Action] -> [OPCODE]
branches2opcodes = concat . map branch2opcode


errOPCODE = [STOP] -- #TODO  [REVERT]   

branch2opcode :: Branch Action -> [OPCODE] 
branch2opcode BZr       = [] 
branch2opcode (BSq i as j) = 
    [JUMPDEST i] ++ actions2opcodes as ++ [PUSHDEST j, JUMP] 
branch2opcode (BIf i cs q as bs j k) = 
    [JUMPDEST i] ++ 
    -- IF 
    actions2opcodes cs ++ [PUSHDEST q, JUMPI] ++
    -- ELSE 
    actions2opcodes bs ++ [PUSHDEST k, JUMP] ++
    -- THEN 
    [JUMPDEST q] ++ actions2opcodes as ++ [PUSHDEST j, JUMP] 
branch2opcode (BDp i dsps) = 
    [JUMPDEST i] ++ loop dsps where 
        loop [] = errOPCODE   -- error handling 
        loop ((a,q,as,j):dsps) = 
            -- DISPATCHER a 
            action2opcode a ++ [PUSHDEST q, JUMPI] ++
            -- ELSE 
            loop dsps ++ 
            -- THEN 
            [JUMPDEST q] ++ actions2opcodes as ++ [PUSHDEST j, JUMP] 
branch2opcode (BCk i chks) = 
    [JUMPDEST i] ++ loop chks where 
        loop []     = errOPCODE -- error handling 
        loop ((a,q,as,j):chks) = 
            -- CHECK a 
            action2opcode a ++ [PUSHDEST q, JUMPI] ++
            -- ELSE 
            loop chks ++ 
            -- THEN 
            [JUMPDEST q] ++ actions2opcodes as ++ [PUSHDEST j, JUMP] 

