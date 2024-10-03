module Knit where

import Prelude hiding (EQ,LT,GT)
import Opcode
import Tree
import Hex
--import Test

bottom  = 0
rev     = reverse 


-- | Cut and Reverse OPCODEs such that 
-- | 1. JUMPDEST and INVALID are separators 
-- | 2. JUMP, RETURN or REVERT can finish the block 
revcut os  = ct os [] where 
    ct  []       blks                   = rev blks   
    ct (JUMPDEST s:os)  bs              = ct os ([]:[JUMPDEST s]:bs)
    ct (INVALID   :os)  bs              = ct os ([]:[INVALID]:bs)
    ct (o         :os) ((JUMP  :b):bs)  = ct os ([o]:(JUMP:b):bs) 
    ct (o         :os) ((JUMPI :b):bs)  = ct os ([o]:(JUMPI:b):bs)
    ct (o         :os) ((SSTORE:b):bs)  = ct os ([o]:(SSTORE:b):bs)
    ct (o         :os) ((MSTORE:b):bs)  = ct os ([o]:(MSTORE:b):bs)
    ct (o         :os) ((POP   :b):bs)  = ct os ([o]:(POP   :b):bs)
    ct (o         :os) ((SWAP1 :b):bs)  = ct os ([o]:(SWAP1 :b):bs)
    ct (o         :os) ((SWAP2 :b):bs)  = ct os ([o]:(SWAP2 :b):bs)
    ct (o         :os) ((SWAP3 :b):bs)  = ct os ([o]:(SWAP3 :b):bs)
    ct (o         :os) ((SWAP4 :b):bs)  = ct os ([o]:(SWAP4 :b):bs)
    ct (o         :os) ((SWAP5 :b):bs)  = ct os ([o]:(SWAP5 :b):bs)
    ct (o         :os) ((DUP1  :b):bs)  = ct os ([o]:(DUP1  :b):bs)
    ct (o         :os) ((DUP2  :b):bs)  = ct os ([o]:(DUP2  :b):bs)
    ct (o         :os) ((DUP3  :b):bs)  = ct os ([o]:(DUP3  :b):bs)
    ct (o         :os) ((DUP4  :b):bs)  = ct os ([o]:(DUP4  :b):bs)
    ct (o         :os) ((DUP5  :b):bs)  = ct os ([o]:(DUP5  :b):bs)
    ct (o         :os) ((CODECOPY:b):bs)= ct os ([o]:(CODECOPY:b):bs)
    ct (o         :os) ((RETURN:b):bs)  = ct os ([o]:(RETURN:b):bs) 
    ct (o         :os) ((REVERT:b):bs)  = ct os ([o]:(REVERT:b):bs) 
    ct (o         :os)  []              = ct os ([o]:[])
    ct (o         :os) (b:bs)           = ct os ((o:b):bs) 


-- | Insert Parenthesises (L,R) 
paren       :: [OPCODE] -> [OPCODE] 
paren ops   = p ops [0] 1 where 
    p []    [_]       i =  []
    p []    [0,n]     i =                     R : p [] [n]         i 
    p []    (0:hd:tl) i =                     R : p [] (hd-1:tl)   i  
    p []    (hd:tl)   i =  L : ARG i        : R : p [] (hd-1:tl)  (i+1) 
    p os    [0,n]     i =                     R : p os [n]         i  
    p os    (0:hd:tl) i =                     R : p os (hd-1:tl)   i   
    p (o:os)(hd:tl)   i = case o of 
        ADD             -> L : ADD              : p os (2:hd  :tl) i         
        MUL             -> L : MUL              : p os (2:hd  :tl) i    
        SUB             -> L : SUB              : p os (2:hd  :tl) i   
        DIV             -> L : SUB              : p os (2:hd  :tl) i   
        SDIV            -> L : SDIV             : p os (2:hd  :tl) i   
        MOD             -> L : MOD              : p os (2:hd  :tl) i   
        SMOD            -> L : SMOD             : p os (2:hd  :tl) i   
        ADDMOD          -> L : ADDMOD           : p os (3:hd  :tl) i   
        MULMOD          -> L : MULMOD           : p os (3:hd  :tl) i   
        EXP             -> L : EXP              : p os (2:hd  :tl) i   
        SIGNEXTEND      -> L : SIGNEXTEND       : p os (2:hd  :tl) i   
        LT              -> L : LT               : p os (2:hd  :tl) i   
        GT              -> L : GT               : p os (2:hd  :tl) i   
        SLT             -> L : SLT              : p os (2:hd  :tl) i   
        SGT             -> L : SGT              : p os (2:hd  :tl) i   
        EQ              -> L : EQ               : p os (2:hd  :tl) i   
        ISZERO          -> L : ISZERO           : p os (1:hd  :tl) i   
        AND             -> L : AND              : p os (2:hd  :tl) i   
        OR              -> L : OR               : p os (2:hd  :tl) i   
        XOR             -> L : XOR              : p os (2:hd  :tl) i   
        NOT             -> L : NOT              : p os (1:hd  :tl) i   
        BYTE            -> L : BYTE             : p os (2:hd  :tl) i   
        SHL             -> L : SHL              : p os (2:hd  :tl) i   
        SHR             -> L : SHR              : p os (2:hd  :tl) i   
        SAR             -> L : SAR              : p os (2:hd  :tl) i   
        BALANCE         -> L : BALANCE          : p os (1:hd  :tl) i   
        CALLDATALOAD    -> L : CALLDATALOAD     : p os (1:hd  :tl) i 
        EXTCODESIZE     -> L : EXTCODESIZE      : p os (1:hd  :tl) i  
        EXTCODEHASH     -> L : EXTCODEHASH      : p os (1:hd  :tl) i  
        BLOCKHASH       -> L : BLOCKHASH        : p os (1:hd  :tl) i  
        MLOAD           -> L : MLOAD            : p os (1:hd  :tl) i  
        SLOAD           -> L : SLOAD            : p os (1:hd  :tl) i  
        CREATE          -> L : CREATE           : p os (3:hd  :tl) i 
        CALL            -> L : CALL             : p os (7:hd  :tl) i
        CALLCODE        -> L : CALLCODE         : p os (7:hd  :tl) i
        DELEGATECALL    -> L : DELEGATECALL     : p os (6:hd  :tl) i
        CREATE2         -> L : CREATE2          : p os (4:hd  :tl) i
        STATICCALL      -> L : STATICCALL       : p os (6:hd  :tl) i
        CALLDATACOPY    -> L : CALLDATACOPY     : p os (3:hd+1:tl) i  
        CODECOPY        -> L : CODECOPY         : p os (3:hd+1:tl) i  
        EXTCODECOPY     -> L : EXTCODECOPY      : p os (4:hd+1:tl) i  
        RETURNDATACOPY  -> L : RETURNDATACOPY   : p os (3:hd+1:tl) i  
        POP             -> L : POP              : p os (1:hd+1:tl) i  
        MSTORE          -> L : MSTORE           : p os (2:hd+1:tl) i  
        MSTORE8         -> L : MSTORE8          : p os (2:hd+1:tl) i  
        SSTORE          -> L : SSTORE           : p os (2:hd+1:tl) i  
        JUMP            -> L : JUMP             : p os (1:hd+1:tl) i          
        JUMPI           -> L : JUMPI            : p os (2:hd+1:tl) i   
        RETURN          -> L : RETURN           : p os (2:hd+1:tl) i 
        REVERT          -> L : REVERT           : p os (2:hd+1:tl) i 
        SELFDESTRUCT    -> L : SELFDESTRUCT     : p os (1:hd+1:tl) i 
        JUMPDEST s      -> L : JUMPDEST s   : R : p os (  hd+1:tl) i     
        STOP            -> L : STOP         : R : p os (  hd  :tl) i       
        ADDRESS         -> L : ADDRESS      : R : p os (  hd-1:tl) i    
        ORIGIN          -> L : ORIGIN       : R : p os (  hd-1:tl) i    
        CALLER          -> L : CALLER       : R : p os (  hd-1:tl) i    
        CALLVALUE       -> L : CALLVALUE    : R : p os (  hd-1:tl) i    
        CALLDATASIZE    -> L : CALLDATASIZE : R : p os (  hd-1:tl) i    
        CODESIZE        -> L : CODESIZE     : R : p os (  hd-1:tl) i  
        GASPRICE        -> L : GASPRICE     : R : p os (  hd-1:tl) i    
        RETURNDATASIZE  -> L : RETURNDATASIZE:R : p os (  hd-1:tl) i    
        COINBASE        -> L : COINBASE     : R : p os (  hd-1:tl) i    
        TIMESTAMP       -> L : TIMESTAMP    : R : p os (  hd-1:tl) i    
        NUMBER          -> L : NUMBER       : R : p os (  hd-1:tl) i    
        DIFFICULTY      -> L : DIFFICULTY   : R : p os (  hd-1:tl) i    
        GASLIMIT        -> L : GASLIMIT     : R : p os (  hd-1:tl) i    
        CHAINID         -> L : CHAINID      : R : p os (  hd-1:tl) i    
        SELFBALANCE     -> L : SELFBALANCE  : R : p os (  hd-1:tl) i    
        PC              -> L : PC           : R : p os (  hd-1:tl) i    
        MSIZE           -> L : MSIZE        : R : p os (  hd-1:tl) i    
        GAS             -> L : GAS          : R : p os (  hd-1:tl) i    
        PUSH1  s        -> L : PUSH1  s     : R : p os (  hd-1:tl) i     
        PUSH2  s        -> L : PUSH2  s     : R : p os (  hd-1:tl) i     
        PUSH3  s        -> L : PUSH3  s     : R : p os (  hd-1:tl) i     
        PUSH4  s        -> L : PUSH4  s     : R : p os (  hd-1:tl) i     
        PUSH5  s        -> L : PUSH5  s     : R : p os (  hd-1:tl) i     
        PUSH6  s        -> L : PUSH6  s     : R : p os (  hd-1:tl) i     
        PUSH7  s        -> L : PUSH7  s     : R : p os (  hd-1:tl) i     
        PUSH8  s        -> L : PUSH8  s     : R : p os (  hd-1:tl) i     
        PUSH9  s        -> L : PUSH9  s     : R : p os (  hd-1:tl) i     
        PUSH10 s        -> L : PUSH10 s     : R : p os (  hd-1:tl) i     
        PUSH11 s        -> L : PUSH11 s     : R : p os (  hd-1:tl) i     
        PUSH12 s        -> L : PUSH12 s     : R : p os (  hd-1:tl) i     
        PUSH13 s        -> L : PUSH13 s     : R : p os (  hd-1:tl) i     
        PUSH14 s        -> L : PUSH14 s     : R : p os (  hd-1:tl) i     
        PUSH15 s        -> L : PUSH15 s     : R : p os (  hd-1:tl) i     
        PUSH16 s        -> L : PUSH16 s     : R : p os (  hd-1:tl) i     
        PUSH17 s        -> L : PUSH17 s     : R : p os (  hd-1:tl) i     
        PUSH18 s        -> L : PUSH18 s     : R : p os (  hd-1:tl) i     
        PUSH19 s        -> L : PUSH19 s     : R : p os (  hd-1:tl) i     
        PUSH20 s        -> L : PUSH20 s     : R : p os (  hd-1:tl) i     
        PUSH21 s        -> L : PUSH21 s     : R : p os (  hd-1:tl) i     
        PUSH22 s        -> L : PUSH22 s     : R : p os (  hd-1:tl) i     
        PUSH23 s        -> L : PUSH23 s     : R : p os (  hd-1:tl) i     
        PUSH24 s        -> L : PUSH24 s     : R : p os (  hd-1:tl) i     
        PUSH25 s        -> L : PUSH25 s     : R : p os (  hd-1:tl) i     
        PUSH26 s        -> L : PUSH26 s     : R : p os (  hd-1:tl) i     
        PUSH27 s        -> L : PUSH27 s     : R : p os (  hd-1:tl) i     
        PUSH28 s        -> L : PUSH28 s     : R : p os (  hd-1:tl) i     
        PUSH29 s        -> L : PUSH29 s     : R : p os (  hd-1:tl) i     
        PUSH30 s        -> L : PUSH30 s     : R : p os (  hd-1:tl) i     
        PUSH31 s        -> L : PUSH31 s     : R : p os (  hd-1:tl) i     
        PUSH32 s        -> L : PUSH32 s     : R : p os (  hd-1:tl) i     
        DUP1            -> L : DUP1         : R : p os (  hd-1:tl) i    
        DUP2            -> L : DUP2         : R : p os (  hd-1:tl) i    
        DUP3            -> L : DUP3         : R : p os (  hd-1:tl) i    
        DUP4            -> L : DUP4         : R : p os (  hd-1:tl) i    
        DUP5            -> L : DUP5         : R : p os (  hd-1:tl) i    
        DUP6            -> L : DUP6         : R : p os (  hd-1:tl) i    
        DUP7            -> L : DUP7         : R : p os (  hd-1:tl) i    
        DUP8            -> L : DUP8         : R : p os (  hd-1:tl) i    
        DUP9            -> L : DUP9         : R : p os (  hd-1:tl) i    
        DUP10           -> L : DUP10        : R : p os (  hd-1:tl) i    
        DUP11           -> L : DUP11        : R : p os (  hd-1:tl) i    
        DUP12           -> L : DUP12        : R : p os (  hd-1:tl) i    
        DUP13           -> L : DUP13        : R : p os (  hd-1:tl) i    
        DUP14           -> L : DUP14        : R : p os (  hd-1:tl) i    
        DUP15           -> L : DUP15        : R : p os (  hd-1:tl) i    
        DUP16           -> L : DUP16        : R : p os (  hd-1:tl) i    
        SWAP1           -> L : SWAP1        : R : p os (  hd  :tl) i  
        SWAP2           -> L : SWAP2        : R : p os (  hd  :tl) i  
        SWAP3           -> L : SWAP3        : R : p os (  hd  :tl) i  
        SWAP4           -> L : SWAP4        : R : p os (  hd  :tl) i  
        SWAP5           -> L : SWAP5        : R : p os (  hd  :tl) i  
        SWAP6           -> L : SWAP6        : R : p os (  hd  :tl) i  
        SWAP7           -> L : SWAP7        : R : p os (  hd  :tl) i  
        SWAP8           -> L : SWAP8        : R : p os (  hd  :tl) i  
        SWAP9           -> L : SWAP9        : R : p os (  hd  :tl) i  
        SWAP10          -> L : SWAP10       : R : p os (  hd  :tl) i  
        SWAP11          -> L : SWAP11       : R : p os (  hd  :tl) i  
        SWAP12          -> L : SWAP12       : R : p os (  hd  :tl) i  
        SWAP13          -> L : SWAP13       : R : p os (  hd  :tl) i  
        SWAP14          -> L : SWAP14       : R : p os (  hd  :tl) i  
        SWAP15          -> L : SWAP15       : R : p os (  hd  :tl) i  
        SWAP16          -> L : SWAP16       : R : p os (  hd  :tl) i  
        INVALID         -> L : INVALID      : R : p os (  hd  :tl) i 
        INFO s          ->                        p os (  hd  :tl) i 



-- | Build Abstract Syntax Tree s.t.
-- | 1. Red Tree has a root OPCODE which returns new value on STACK
-- | 2. Black Tree has a root which does not put new value on STACK
knitT :: [OPCODE]  -> (RBTree OPCODE, [OPCODE]) 
knitT opcodes = 
    let r = rev   in 
    let f = knitF in 
    case opcodes of 
    STOP            : os -> (BLK STOP           (r ags), cnt)     where (ags,cnt) = f os
    REVERT          : os -> (BLK REVERT         (r ags), cnt)     where (ags,cnt) = f os
    RETURN          : os -> (BLK RETURN         (r ags), cnt)     where (ags,cnt) = f os
    CALLDATACOPY    : os -> (BLK CALLDATACOPY   (r ags), cnt)     where (ags,cnt) = f os
    CODECOPY        : os -> (BLK CODECOPY       (r ags), cnt)     where (ags,cnt) = f os
    EXTCODECOPY     : os -> (BLK EXTCODECOPY    (r ags), cnt)     where (ags,cnt) = f os
    RETURNDATACOPY  : os -> (BLK RETURNDATACOPY (r ags), cnt)     where (ags,cnt) = f os
    POP             : os -> (BLK POP            (r ags), cnt)     where (ags,cnt) = f os
    MSTORE          : os -> (BLK MSTORE         (r ags), cnt)     where (ags,cnt) = f os
    MSTORE8         : os -> (BLK MSTORE8        (r ags), cnt)     where (ags,cnt) = f os
    SSTORE          : os -> (BLK SSTORE         (r ags), cnt)     where (ags,cnt) = f os
    JUMP            : os -> (BLK JUMP           (r ags), cnt)     where (ags,cnt) = f os
    JUMPI           : os -> (BLK JUMPI          (r ags), cnt)     where (ags,cnt) = f os
    JUMPDEST s      : os -> (BLK (JUMPDEST s)   (r ags), cnt)     where (ags,cnt) = f os
    SWAP1           : os -> (BLK SWAP1          (r ags), cnt)     where (ags,cnt) = f os
    SWAP2           : os -> (BLK SWAP2          (r ags), cnt)     where (ags,cnt) = f os
    SWAP3           : os -> (BLK SWAP3          (r ags), cnt)     where (ags,cnt) = f os
    SWAP4           : os -> (BLK SWAP4          (r ags), cnt)     where (ags,cnt) = f os
    SWAP5           : os -> (BLK SWAP5          (r ags), cnt)     where (ags,cnt) = f os
    SWAP6           : os -> (BLK SWAP6          (r ags), cnt)     where (ags,cnt) = f os
    SWAP7           : os -> (BLK SWAP7          (r ags), cnt)     where (ags,cnt) = f os
    SWAP8           : os -> (BLK SWAP8          (r ags), cnt)     where (ags,cnt) = f os
    SWAP9           : os -> (BLK SWAP9          (r ags), cnt)     where (ags,cnt) = f os
    SWAP10          : os -> (BLK SWAP10         (r ags), cnt)     where (ags,cnt) = f os
    SWAP11          : os -> (BLK SWAP11         (r ags), cnt)     where (ags,cnt) = f os
    SWAP12          : os -> (BLK SWAP12         (r ags), cnt)     where (ags,cnt) = f os
    SWAP13          : os -> (BLK SWAP13         (r ags), cnt)     where (ags,cnt) = f os
    SWAP14          : os -> (BLK SWAP14         (r ags), cnt)     where (ags,cnt) = f os
    SWAP15          : os -> (BLK SWAP15         (r ags), cnt)     where (ags,cnt) = f os
    SWAP16          : os -> (BLK SWAP16         (r ags), cnt)     where (ags,cnt) = f os
    INVALID         : os -> (BLK INVALID        (r ags), cnt)     where (ags,cnt) = f os 
    o               : os -> (RED o              (r ags), cnt)     where (ags,cnt) = f os 



knitF :: [OPCODE] -> ([RBTree OPCODE], [OPCODE]) 
knitF opcodes = loop opcodes 1 where 
    loop opcodes i = case opcodes of 
        L : ops             ->  let (ret,cont)   = knitT ops in 
                                let (rest,cont') = loop cont i in (ret:rest, cont') 
        R : ops             ->  ([], ops) 
        e : ops             ->  error (show e) 
        []                  ->  ([], []) 



knit        :: [OPCODE] -> [RBTree OPCODE] 
knit        = rev . fst . knitF . paren   

-- e.g. 
-- λ> knit . rev $ [ADD, SUB, MUL, PUSH1 "00", EQ] 
-- [
-- +-  EQ
--     +-  MUL
--     |   +-  ARG 4
--     |   +-  SUB
--     |       +-  ARG 3
--     |       +-  ADD
--     |           +-  ARG 2
--     |           +-  ARG 1
--     +-  PUSH1 "00"



-- combine [JUMPDEST] and the next block 
cat         :: [[RBTree OPCODE]] -> [[RBTree OPCODE]]  
cat []                                          = []
cat ([BLK INVALID _]:xs)                        = cat xs 
cat ([BLK(JUMPDEST s)_] : seq : xs)  = [BLK SEQ (BLK(JUMPDEST s)[] : seq)] : cat xs  
cat (x:xs)                           = x : cat xs 

partition :: [RBTree OPCODE] -> [RBTree OPCODE] 
partition trs = loop trs [] where
    loop []                     trs = [BLK SEQ (reverse trs)]   
    loop (BLK(JUMPDEST s)_: xs) trs =  BLK SEQ (reverse trs) : loop xs [BLK (JUMPDEST s)[]] 
    loop (BLK JUMPI subtr : xs) trs =  BLK SEQ (reverse trs) : loop xs [BLK JUMPI subtr]   
    loop (x               : xs) trs = loop xs (x:trs) 

knits       :: [[OPCODE]] -> [RBTree OPCODE] 
knits       = partition . concat {--. cat --}. map knit


