module VM where

import Prelude hiding (EQ,LT,GT)
import Asm
import Tree

rev = reverse 


paren ops = par (rev ops) [-2]
 
par                 :: [OPCODE] -> [Int] -> [OPCODE] 
par [EOF] [-2]      = []
par []   (hd:tl)    = par [EOF] (hd:tl)
par asms (hd:tl)    = 
    if hd == -1  then error "arg counting stack cannot be -1" else   
    if hd == 0   then case tl of 
                    [-2]    -> R : par [EOF] [-2] 
                    t:ts    -> R : par asms (t-1:ts) else case asms of 
    STOP            : os    -> L : STOP       : R : par os (hd:tl)
    ADD             : os    -> L : ADD            : par os (2:hd:tl)      
    MUL             : os    -> L : MUL            : par os (2:hd:tl) 
    SUB             : os    -> L : SUB            : par os (2:hd:tl)
    DIV             : os    -> L : SUB            : par os (2:hd:tl)
    SDIV            : os    -> L : SDIV           : par os (2:hd:tl)
    MOD             : os    -> L : MOD            : par os (2:hd:tl)
    SMOD            : os    -> L : SMOD           : par os (2:hd:tl)
    ADDMOD          : os    -> L : ADDMOD         : par os (3:hd:tl)
    MULMOD          : os    -> L : MULMOD         : par os (3:hd:tl)
    EXP             : os    -> L : EXP            : par os (2:hd:tl)
    SIGNEXTEND      : os    -> L : SIGNEXTEND     : par os (2:hd:tl)
    LT              : os    -> L : LT             : par os (2:hd:tl)
    GT              : os    -> L : GT             : par os (2:hd:tl)
    SLT             : os    -> L : SLT            : par os (2:hd:tl)
    SGT             : os    -> L : SGT            : par os (2:hd:tl)
    EQ              : os    -> L : EQ             : par os (2:hd:tl)
    ISZERO          : os    -> L : ISZERO         : par os (1:hd:tl)
    AND             : os    -> L : AND            : par os (2:hd:tl)
    OR              : os    -> L : OR             : par os (2:hd:tl)
    XOR             : os    -> L : XOR            : par os (2:hd:tl)
    NOT             : os    -> L : NOT            : par os (1:hd:tl)
    BYTE            : os    -> L : BYTE           : par os (2:hd:tl)
    SHL             : os    -> L : SHL            : par os (2:hd:tl)
    SHR             : os    -> L : SHR            : par os (2:hd:tl)
    SAR             : os    -> L : SAR            : par os (2:hd:tl)
    ADDRESS         : os    -> L : ADDRESS  : R   : par os (hd-1:tl)
    BALANCE         : os    -> L : BALANCE        : par os (1:hd:tl)
    ORIGIN          : os    -> L : ORIGIN   : R   : par os (hd-1:tl)
    CALLER          : os    -> L : CALLER   : R   : par os (hd-1:tl)
    CALLVALUE       : os    -> L : CALLVALUE : R  : par os (hd-1:tl)
    CALLDATASIZE    : os    -> L : CALLDATASIZE   : par os (1:hd:tl)
    CALLDATACOPY    : os    -> L : CALLDATACOPY   : par os (3:hd+1:tl)
    PUSH1  s        : os    -> L : PUSH1  s : R   : par os (hd-1:tl) 
    PUSH2  s        : os    -> L : PUSH2  s : R   : par os (hd-1:tl) 
    PUSH3  s        : os    -> L : PUSH3  s : R   : par os (hd-1:tl) 
    PUSH4  s        : os    -> L : PUSH4  s : R   : par os (hd-1:tl) 
    PUSH5  s        : os    -> L : PUSH5  s : R   : par os (hd-1:tl) 
    PUSH6  s        : os    -> L : PUSH6  s : R   : par os (hd-1:tl) 
    PUSH7  s        : os    -> L : PUSH7  s : R   : par os (hd-1:tl) 
    PUSH8  s        : os    -> L : PUSH8  s : R   : par os (hd-1:tl) 
    PUSH9  s        : os    -> L : PUSH9  s : R   : par os (hd-1:tl) 
    PUSH10 s        : os    -> L : PUSH10 s : R   : par os (hd-1:tl) 
    PUSH11 s        : os    -> L : PUSH11 s : R   : par os (hd-1:tl) 
    PUSH12 s        : os    -> L : PUSH12 s : R   : par os (hd-1:tl) 
    PUSH13 s        : os    -> L : PUSH13 s : R   : par os (hd-1:tl) 
    PUSH14 s        : os    -> L : PUSH14 s : R   : par os (hd-1:tl) 
    PUSH15 s        : os    -> L : PUSH15 s : R   : par os (hd-1:tl) 
    PUSH16 s        : os    -> L : PUSH16 s : R   : par os (hd-1:tl) 
    PUSH17 s        : os    -> L : PUSH17 s : R   : par os (hd-1:tl) 
    PUSH18 s        : os    -> L : PUSH18 s : R   : par os (hd-1:tl) 
    PUSH19 s        : os    -> L : PUSH19 s : R   : par os (hd-1:tl) 
    PUSH20 s        : os    -> L : PUSH20 s : R   : par os (hd-1:tl) 
    PUSH21 s        : os    -> L : PUSH21 s : R   : par os (hd-1:tl) 
    PUSH22 s        : os    -> L : PUSH22 s : R   : par os (hd-1:tl) 
    PUSH23 s        : os    -> L : PUSH23 s : R   : par os (hd-1:tl) 
    PUSH24 s        : os    -> L : PUSH24 s : R   : par os (hd-1:tl) 
    PUSH25 s        : os    -> L : PUSH25 s : R   : par os (hd-1:tl) 
    PUSH26 s        : os    -> L : PUSH26 s : R   : par os (hd-1:tl) 
    PUSH27 s        : os    -> L : PUSH27 s : R   : par os (hd-1:tl) 
    PUSH28 s        : os    -> L : PUSH28 s : R   : par os (hd-1:tl) 
    PUSH29 s        : os    -> L : PUSH29 s : R   : par os (hd-1:tl) 
    PUSH30 s        : os    -> L : PUSH30 s : R   : par os (hd-1:tl) 
    PUSH31 s        : os    -> L : PUSH31 s : R   : par os (hd-1:tl) 
    PUSH32 s        : os    -> L : PUSH32 s : R   : par os (hd-1:tl) 
    EOF             : os    ->                      par(EOF:os)(hd:tl) 

prog = 
    [PUSH1 "1", PUSH1 "2", ADD, 
     PUSH1 "3", PUSH1 "4", SUB, 
     ADD,
     PUSH1 "1", PUSH1 "2", ADD, 
     PUSH1 "3", PUSH1 "4", SUB, 
     MUL, 
     STOP, 
     SUB]
prog1 = 
    [PUSH1 "9",
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


splitT :: [OPCODE]  -> (RBTree OPCODE, [OPCODE]) 
splitT opcodes = 
    let r = rev in 
    let f = splitF in 
    case opcodes of 
    STOP            : os -> (BLK STOP         (r ags), cnt)     where (ags,cnt) = f os
    ADD             : os -> (RED ADD          (r ags), cnt)     where (ags,cnt) = f os
    SUB             : os -> (RED SUB          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    DIV             : os -> (RED DIV          (r ags), cnt)     where (ags,cnt) = f os
    SDIV            : os -> (RED SDIV         (r ags), cnt)     where (ags,cnt) = f os
    MOD             : os -> (RED MOD          (r ags), cnt)     where (ags,cnt) = f os
    SMOD            : os -> (RED SMOD         (r ags), cnt)     where (ags,cnt) = f os
    ADDMOD          : os -> (RED ADDMOD       (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    MUL             : os -> (RED MUL          (r ags), cnt)     where (ags,cnt) = f os
    ADDRESS         : os -> (RED ADDRESS      (r ags), cnt)     where (ags,cnt) = f os
    CALLDATACOPY    : os -> (BLK CALLDATACOPY (r ags), cnt)     where (ags,cnt) = f os
    PUSH1  s        : os -> (RED (PUSH1  s)       ret, cnt)     where (ret,cnt) = f os
    PUSH2  s        : os -> (RED (PUSH2  s)       ret, cnt)     where (ret,cnt) = f os
    PUSH3  s        : os -> (RED (PUSH3  s)       ret, cnt)     where (ret,cnt) = f os
    PUSH4  s        : os -> (RED (PUSH4  s)       ret, cnt)     where (ret,cnt) = f os
    PUSH5  s        : os -> (RED (PUSH5  s)       ret, cnt)     where (ret,cnt) = f os
    PUSH6  s        : os -> (RED (PUSH6  s)       ret, cnt)     where (ret,cnt) = f os
    PUSH7  s        : os -> (RED (PUSH7  s)       ret, cnt)     where (ret,cnt) = f os
    PUSH8  s        : os -> (RED (PUSH8  s)       ret, cnt)     where (ret,cnt) = f os
    PUSH9  s        : os -> (RED (PUSH9  s)       ret, cnt)     where (ret,cnt) = f os
    PUSH10 s        : os -> (RED (PUSH10 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH11 s        : os -> (RED (PUSH11 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH12 s        : os -> (RED (PUSH12 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH13 s        : os -> (RED (PUSH13 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH14 s        : os -> (RED (PUSH14 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH15 s        : os -> (RED (PUSH15 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH16 s        : os -> (RED (PUSH16 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH17 s        : os -> (RED (PUSH17 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH18 s        : os -> (RED (PUSH18 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH19 s        : os -> (RED (PUSH19 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH20 s        : os -> (RED (PUSH20 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH21 s        : os -> (RED (PUSH21 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH22 s        : os -> (RED (PUSH22 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH23 s        : os -> (RED (PUSH23 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH24 s        : os -> (RED (PUSH24 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH25 s        : os -> (RED (PUSH25 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH26 s        : os -> (RED (PUSH26 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH27 s        : os -> (RED (PUSH27 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH28 s        : os -> (RED (PUSH28 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH29 s        : os -> (RED (PUSH29 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH30 s        : os -> (RED (PUSH30 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH31 s        : os -> (RED (PUSH31 s)       ret, cnt)     where (ret,cnt) = f os
    PUSH32 s        : os -> (RED (PUSH32 s)       ret, cnt)     where (ret,cnt) = f os

splitF opcodes = case opcodes of 
    L : ops         -> let (ret,cont)   = splitT ops in 
                       let (rest,cont') = splitF cont in (ret:rest, cont') 
    R : ops         -> ([], ops) 
    e : ops         -> error (show e) 
    []              -> ([], []) 

    
parse = splitF . paren 

    
    


