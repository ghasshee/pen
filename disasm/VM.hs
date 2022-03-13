module VM where

import Prelude hiding (EQ,LT,GT)
import Asm
import Tree

bottom  = 0
rev     = reverse 

cut []       blks               = rev blks   
cut(o:os) ((JUMP    :b):bs)     = cut os ([o]:(JUMP:b):bs) 
cut(o:os) ((RETURN  :b):bs)     = cut os ([o]:(RETURN:b):bs) 
cut(o:os) ((REVERT  :b):bs)     = cut os ([o]:(REVERT:b):bs) 
cut(o:os) ([JUMPDEST s]  :bs)   = cut os ([o]:[JUMPDEST s]:bs)
cut(o:os) ((JUMPDEST s:b):bs)   = cut os ([o]:[JUMPDEST s]:b:bs)
cut(o:os) ([INVALID]   :bs)     = cut os ([o]:[INVALID]:bs)
cut(o:os) ((INVALID :b):bs)     = cut os ([o]:[INVALID]:b:bs)
cut(o:os) []                    = cut os ([o]:[])
cut(o:os) ([]:bs)               = cut os ([o]:bs) 
cut(o:os) (b:bs)                = cut os ((o:b):bs) 

fparen blks = fmap paren blks 

paren ops = par ops [0]
 
par                 :: [OPCODE] -> [Int] -> [OPCODE]
par []  _           = []
par ops [0,n]       = R  : par ops [n]      
par ops (0:t:ts)    = R  : par ops (t-1:ts) 
par (o:os)(hd:tl)   = let f = par os in case o of 
    STOP            -> L : STOP       : R   : f (hd:tl)      
    ADD             -> L : ADD              : f (2:hd:tl)         
    MUL             -> L : MUL              : f (2:hd:tl)    
    SUB             -> L : SUB              : f (2:hd:tl)   
    DIV             -> L : SUB              : f (2:hd:tl)   
    SDIV            -> L : SDIV             : f (2:hd:tl)   
    MOD             -> L : MOD              : f (2:hd:tl)   
    SMOD            -> L : SMOD             : f (2:hd:tl)   
    ADDMOD          -> L : ADDMOD           : f (3:hd:tl)   
    MULMOD          -> L : MULMOD           : f (3:hd:tl)   
    EXP             -> L : EXP              : f (2:hd:tl)   
    SIGNEXTEND      -> L : SIGNEXTEND       : f (2:hd:tl)   
    LT              -> L : LT               : f (2:hd:tl)   
    GT              -> L : GT               : f (2:hd:tl)   
    SLT             -> L : SLT              : f (2:hd:tl)   
    SGT             -> L : SGT              : f (2:hd:tl)   
    EQ              -> L : EQ               : f (2:hd:tl)   
    ISZERO          -> L : ISZERO           : f (1:hd:tl)   
    AND             -> L : AND              : f (2:hd:tl)   
    OR              -> L : OR               : f (2:hd:tl)   
    XOR             -> L : XOR              : f (2:hd:tl)   
    NOT             -> L : NOT              : f (1:hd:tl)   
    BYTE            -> L : BYTE             : f (2:hd:tl)   
    SHL             -> L : SHL              : f (2:hd:tl)   
    SHR             -> L : SHR              : f (2:hd:tl)   
    SAR             -> L : SAR              : f (2:hd:tl)   
    ADDRESS         -> L : ADDRESS      : R : f (hd-1:tl)   
    BALANCE         -> L : BALANCE          : f (1:hd:tl)   
    ORIGIN          -> L : ORIGIN       : R : f (hd-1:tl)   
    CALLER          -> L : CALLER       : R : f (hd-1:tl)   
    CALLVALUE       -> L : CALLVALUE    : R : f (hd-1:tl)   
    CALLDATASIZE    -> L : CALLDATASIZE : R : f (hd-1:tl)   
    CALLDATACOPY    -> L : CALLDATACOPY     : f (3:hd+1:tl) 
    CALLDATALOAD    -> L : CALLDATALOAD     : f (1:hd:tl) 
    CODESIZE        -> L : CODESIZE     : R : f (hd-1:tl) 
    CODECOPY        -> L : CODECOPY         : f (3:hd+1:tl) 
    GASPRICE        -> L : GASPRICE     : R : f (hd-1:tl)   
    EXTCODESIZE     -> L : EXTCODESIZE      : f (1:hd:tl)   
    EXTCODECOPY     -> L : EXTCODECOPY      : f (4:hd+1:tl) 
    RETURNDATASIZE  -> L : RETURNDATASIZE:R : f (hd-1:tl)   
    RETURNDATACOPY  -> L : RETURNDATACOPY   : f (3:hd+1:tl) 
    EXTCODEHASH     -> L : EXTCODEHASH      : f (1:hd:tl)   
    BLOCKHASH       -> L : BLOCKHASH        : f (1:hd:tl)   
    COINBASE        -> L : COINBASE     : R : f (hd-1:tl)   
    TIMESTAMP       -> L : TIMESTAMP    : R : f (hd-1:tl)   
    NUMBER          -> L : NUMBER       : R : f (hd-1:tl)   
    DIFFICULTY      -> L : DIFFICULTY   : R : f (hd-1:tl)   
    GASLIMIT        -> L : GASLIMIT     : R : f (hd-1:tl)   
    CHAINID         -> L : CHAINID      : R : f (hd-1:tl)   
    SELFBALANCE     -> L : SELFBALANCE  : R : f (hd-1:tl)   
    POP             -> L : POP              : f (1:hd+1:tl) 
    MLOAD           -> L : MLOAD            : f (1:hd:tl)   
    MSTORE          -> L : MSTORE           : f (2:hd+1:tl) 
    MSTORE8         -> L : MSTORE8          : f (2:hd+1:tl) 
    SLOAD           -> L : SLOAD            : f (1:hd:tl)   
    SSTORE          -> L : SSTORE           : f (2:hd+1:tl) 
    JUMP            -> L : JUMP             : f (1:hd+1:tl)         
    JUMPI           -> L : JUMPI            : f (2:hd+1:tl)  
    JUMPDEST s      -> L : JUMPDEST s   : R : f (hd+1:tl)    
    PC              -> L : PC           : R : f (hd-1:tl)   
    MSIZE           -> L : MSIZE        : R : f (hd-1:tl)   
    GAS             -> L : GAS          : R : f (hd-1:tl)   
    PUSH1  s        -> L : PUSH1  s     : R : f (hd-1:tl)    
    PUSH2  s        -> L : PUSH2  s     : R : f (hd-1:tl)    
    PUSH3  s        -> L : PUSH3  s     : R : f (hd-1:tl)    
    PUSH4  s        -> L : PUSH4  s     : R : f (hd-1:tl)    
    PUSH5  s        -> L : PUSH5  s     : R : f (hd-1:tl)    
    PUSH6  s        -> L : PUSH6  s     : R : f (hd-1:tl)    
    PUSH7  s        -> L : PUSH7  s     : R : f (hd-1:tl)    
    PUSH8  s        -> L : PUSH8  s     : R : f (hd-1:tl)    
    PUSH9  s        -> L : PUSH9  s     : R : f (hd-1:tl)    
    PUSH10 s        -> L : PUSH10 s     : R : f (hd-1:tl)    
    PUSH11 s        -> L : PUSH11 s     : R : f (hd-1:tl)    
    PUSH12 s        -> L : PUSH12 s     : R : f (hd-1:tl)    
    PUSH13 s        -> L : PUSH13 s     : R : f (hd-1:tl)    
    PUSH14 s        -> L : PUSH14 s     : R : f (hd-1:tl)    
    PUSH15 s        -> L : PUSH15 s     : R : f (hd-1:tl)    
    PUSH16 s        -> L : PUSH16 s     : R : f (hd-1:tl)    
    PUSH17 s        -> L : PUSH17 s     : R : f (hd-1:tl)    
    PUSH18 s        -> L : PUSH18 s     : R : f (hd-1:tl)    
    PUSH19 s        -> L : PUSH19 s     : R : f (hd-1:tl)    
    PUSH20 s        -> L : PUSH20 s     : R : f (hd-1:tl)    
    PUSH21 s        -> L : PUSH21 s     : R : f (hd-1:tl)    
    PUSH22 s        -> L : PUSH22 s     : R : f (hd-1:tl)    
    PUSH23 s        -> L : PUSH23 s     : R : f (hd-1:tl)    
    PUSH24 s        -> L : PUSH24 s     : R : f (hd-1:tl)    
    PUSH25 s        -> L : PUSH25 s     : R : f (hd-1:tl)    
    PUSH26 s        -> L : PUSH26 s     : R : f (hd-1:tl)    
    PUSH27 s        -> L : PUSH27 s     : R : f (hd-1:tl)    
    PUSH28 s        -> L : PUSH28 s     : R : f (hd-1:tl)    
    PUSH29 s        -> L : PUSH29 s     : R : f (hd-1:tl)    
    PUSH30 s        -> L : PUSH30 s     : R : f (hd-1:tl)    
    PUSH31 s        -> L : PUSH31 s     : R : f (hd-1:tl)    
    PUSH32 s        -> L : PUSH32 s     : R : f (hd-1:tl)    
    DUP1            -> L : DUP1         : R : f (hd-1:tl)   
    DUP2            -> L : DUP2         : R : f (hd-1:tl)   
    DUP3            -> L : DUP3         : R : f (hd-1:tl)   
    DUP4            -> L : DUP4         : R : f (hd-1:tl)   
    DUP5            -> L : DUP5         : R : f (hd-1:tl)   
    DUP6            -> L : DUP6         : R : f (hd-1:tl)   
    DUP7            -> L : DUP7         : R : f (hd-1:tl)   
    DUP8            -> L : DUP8         : R : f (hd-1:tl)   
    DUP9            -> L : DUP9         : R : f (hd-1:tl)   
    DUP10           -> L : DUP10        : R : f (hd-1:tl)   
    DUP11           -> L : DUP11        : R : f (hd-1:tl)   
    DUP12           -> L : DUP12        : R : f (hd-1:tl)   
    DUP13           -> L : DUP13        : R : f (hd-1:tl)   
    DUP14           -> L : DUP14        : R : f (hd-1:tl)   
    DUP15           -> L : DUP15        : R : f (hd-1:tl)   
    DUP16           -> L : DUP16        : R : f (hd-1:tl)   
    SWAP1           -> L : SWAP1        : R : f (hd+1:tl)   
    SWAP2           -> L : SWAP2        : R : f (hd+1:tl)   
    SWAP3           -> L : SWAP3        : R : f (hd+1:tl)   
    SWAP4           -> L : SWAP4        : R : f (hd+1:tl)   
    SWAP5           -> L : SWAP5        : R : f (hd+1:tl)   
    SWAP6           -> L : SWAP6        : R : f (hd+1:tl)   
    SWAP7           -> L : SWAP7        : R : f (hd+1:tl)   
    SWAP8           -> L : SWAP8        : R : f (hd+1:tl)   
    SWAP9           -> L : SWAP9        : R : f (hd+1:tl)   
    SWAP10          -> L : SWAP10       : R : f (hd+1:tl)   
    SWAP11          -> L : SWAP11       : R : f (hd+1:tl)   
    SWAP12          -> L : SWAP12       : R : f (hd+1:tl)   
    SWAP13          -> L : SWAP13       : R : f (hd+1:tl)   
    SWAP14          -> L : SWAP14       : R : f (hd+1:tl)   
    SWAP15          -> L : SWAP15       : R : f (hd+1:tl)   
    SWAP16          -> L : SWAP16       : R : f (hd+1:tl)   
    CREATE          -> L : CREATE           : f (3:hd:tl) 
    CALL            -> L : CALL             : f (7:hd:tl)
    CALLCODE        -> L : CALLCODE         : f (7:hd:tl)
    RETURN          -> L : RETURN           : f (2:hd+1:tl)
    DELEGATECALL    -> L : DELEGATECALL     : f (6:hd:tl)
    CREATE2         -> L : CREATE2          : f (4:hd:tl) 
    STATICCALL      -> L : STATICCALL       : f (6:hd:tl)
    REVERT          -> L : REVERT           : f (2:hd+1:tl)
    INVALID         ->                        f (hd:tl)  
    SELFDESTRUCT    -> L : SELFDESTRUCT     : f (1:hd+1:tl)
    INFO s          ->                        f (hd:tl)  
    EOF             ->                        par(EOF:os)(hd:tl) 

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


splitT :: [OPCODE]  -> (RBTree OPCODE, [OPCODE]) 
splitT opcodes = 
    let r = rev in 
    let f = splitF in 
    case opcodes of 
    STOP            : os -> (BLK STOP         (r ags), cnt)     where (ags,cnt) = f os
    CALLDATACOPY    : os -> (BLK CALLDATACOPY (r ags), cnt)     where (ags,cnt) = f os
    CODECOPY        : os -> (BLK CODECOPY     (r ags), cnt)     where (ags,cnt) = f os
    EXTCODECOPY     : os -> (BLK EXTCODECOPY  (r ags), cnt)     where (ags,cnt) = f os
    RETURNDATACOPY  : os -> (BLK RETURNDATACOPY(r as), cnt)     where ( as,cnt) = f os
    POP             : os -> (BLK POP          (r ags), cnt)     where (ags,cnt) = f os
    MSTORE          : os -> (BLK MSTORE       (r ags), cnt)     where (ags,cnt) = f os
    MSTORE8         : os -> (BLK MSTORE8      (r ags), cnt)     where (ags,cnt) = f os
    SSTORE          : os -> (BLK SSTORE       (r ags), cnt)     where (ags,cnt) = f os
    JUMP            : os -> (BLK JUMP         (r ags), cnt)     where (ags,cnt) = f os
    JUMPI           : os -> (BLK JUMPI        (r ags), cnt)     where (ags,cnt) = f os
    JUMPDEST s      : os -> (BLK (JUMPDEST s) (r ags), cnt)     where (ags,cnt) = f os
    o               : os -> (RED o            (r ags), cnt)     where (ags,cnt) = f os 


splitF opcodes = case opcodes of 
    L : ops         -> let (ret,cont)   = splitT ops in 
                       let (rest,cont') = splitF cont in (ret:rest, cont') 
    R : ops         -> ([], ops) 
    e : ops         -> error (show e) 
    []              -> ([], []) 

   
cuts ops = cut ops [] 
parse prog = rev . fst . splitF . paren $ prog 

fparse progs = fmap parse progs

    
    


