module Test where 

import Asm


-- e.g. for test 
prog = 
    [PUSH1 "1", PUSH1 "2", ADD, 
     PUSH1 "3", PUSH1 "4", SUB, 
     ADD,
     PUSH1 "1", PUSH1 "2", ADD, 
     STOP, 
     PUSH1 "3", PUSH1 "4", SUB, 
     MUL, 
     POP, 
     PUSH1 "10",
     SUB,
     JUMP,
     PUSH1 "9",
     PUSH1 "1", PUSH1 "2", ADD, 
     PUSH1 "3", PUSH1 "4", SUB, 
     PUSH1 "7",
     CALLDATACOPY, 
     PUSH1 "1", PUSH1 "2", ADD, 
     PUSH1 "3", PUSH1 "4", SUB, 
     MUL, 
     SUB]
prog2 = 
    [PUSH1 "1", PUSH1 "2", ADD, 
     PUSH1 "3", 
     ADD]

