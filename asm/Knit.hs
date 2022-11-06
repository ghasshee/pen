module Knit where

import Prelude hiding (EQ,LT,GT)
import Opcode
import Tree
import Hex
--import Test

bottom  = 0
rev     = reverse 


-- | Cut OPCODEs such that 
-- | 1. JUMPDEST and INVALID are separators 
-- | 2. JUMP, RETURN or REVERT can finish the block 
cut os  = ct os [] where 
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
paren ops   = p ops [0] where 
    p []    [_]         =  []
    p []    [0,n]       =                     R : p [] [n]       
    p []    (0:hd:tl)   =                     R : p [] (hd-1:tl)  
    p []    (hd:tl)     =  L : STACK        : R : p [] (hd-1:tl) 
    p os    [0,n]       =                     R : p os [n]        
    p os    (0:hd:tl)   =                     R : p os (hd-1:tl)   
    p (o:os)(hd:tl)     = case o of 
        ADD             -> L : ADD              : p os (2:hd:tl)         
        MUL             -> L : MUL              : p os (2:hd:tl)    
        SUB             -> L : SUB              : p os (2:hd:tl)   
        DIV             -> L : SUB              : p os (2:hd:tl)   
        SDIV            -> L : SDIV             : p os (2:hd:tl)   
        MOD             -> L : MOD              : p os (2:hd:tl)   
        SMOD            -> L : SMOD             : p os (2:hd:tl)   
        ADDMOD          -> L : ADDMOD           : p os (3:hd:tl)   
        MULMOD          -> L : MULMOD           : p os (3:hd:tl)   
        EXP             -> L : EXP              : p os (2:hd:tl)   
        SIGNEXTEND      -> L : SIGNEXTEND       : p os (2:hd:tl)   
        LT              -> L : LT               : p os (2:hd:tl)   
        GT              -> L : GT               : p os (2:hd:tl)   
        SLT             -> L : SLT              : p os (2:hd:tl)   
        SGT             -> L : SGT              : p os (2:hd:tl)   
        EQ              -> L : EQ               : p os (2:hd:tl)   
        ISZERO          -> L : ISZERO           : p os (1:hd:tl)   
        AND             -> L : AND              : p os (2:hd:tl)   
        OR              -> L : OR               : p os (2:hd:tl)   
        XOR             -> L : XOR              : p os (2:hd:tl)   
        NOT             -> L : NOT              : p os (1:hd:tl)   
        BYTE            -> L : BYTE             : p os (2:hd:tl)   
        SHL             -> L : SHL              : p os (2:hd:tl)   
        SHR             -> L : SHR              : p os (2:hd:tl)   
        SAR             -> L : SAR              : p os (2:hd:tl)   
        BALANCE         -> L : BALANCE          : p os (1:hd:tl)   
        CALLDATACOPY    -> L : CALLDATACOPY     : p os (3:hd+1:tl) 
        CALLDATALOAD    -> L : CALLDATALOAD     : p os (1:hd:tl) 
        CODECOPY        -> L : CODECOPY         : p os (3:hd+1:tl) 
        EXTCODESIZE     -> L : EXTCODESIZE      : p os (1:hd:tl)   
        EXTCODECOPY     -> L : EXTCODECOPY      : p os (4:hd+1:tl) 
        RETURNDATACOPY  -> L : RETURNDATACOPY   : p os (3:hd+1:tl) 
        EXTCODEHASH     -> L : EXTCODEHASH      : p os (1:hd:tl)   
        BLOCKHASH       -> L : BLOCKHASH        : p os (1:hd:tl)   
        POP             -> L : POP              : p os (1:hd+1:tl) 
        MLOAD           -> L : MLOAD            : p os (1:hd:tl)   
        MSTORE          -> L : MSTORE           : p os (2:hd+1:tl) 
        MSTORE8         -> L : MSTORE8          : p os (2:hd+1:tl) 
        SLOAD           -> L : SLOAD            : p os (1:hd:tl)   
        SSTORE          -> L : SSTORE           : p os (2:hd+1:tl) 
        JUMP            -> L : JUMP             : p os (1:hd+1:tl)         
        JUMPI           -> L : JUMPI            : p os (2:hd+1:tl)  
        CREATE          -> L : CREATE           : p os (3:hd:tl) 
        CALL            -> L : CALL             : p os (7:hd:tl)
        CALLCODE        -> L : CALLCODE         : p os (7:hd:tl)
        RETURN          -> L : RETURN           : p os (2:hd+1:tl)
        DELEGATECALL    -> L : DELEGATECALL     : p os (6:hd:tl)
        CREATE2         -> L : CREATE2          : p os (4:hd:tl) 
        STATICCALL      -> L : STATICCALL       : p os (6:hd:tl)
        REVERT          -> L : REVERT           : p os (2:hd+1:tl)
        SELFDESTRUCT    -> L : SELFDESTRUCT     : p os (1:hd+1:tl)
        STOP            -> L : STOP         : R : p os (  hd:tl)      
        ADDRESS         -> L : ADDRESS      : R : p os (hd-1:tl)   
        ORIGIN          -> L : ORIGIN       : R : p os (hd-1:tl)   
        CALLER          -> L : CALLER       : R : p os (hd-1:tl)   
        CALLVALUE       -> L : CALLVALUE    : R : p os (hd-1:tl)   
        CALLDATASIZE    -> L : CALLDATASIZE : R : p os (hd-1:tl)   
        CODESIZE        -> L : CODESIZE     : R : p os (hd-1:tl) 
        GASPRICE        -> L : GASPRICE     : R : p os (hd-1:tl)   
        RETURNDATASIZE  -> L : RETURNDATASIZE:R : p os (hd-1:tl)   
        COINBASE        -> L : COINBASE     : R : p os (hd-1:tl)   
        TIMESTAMP       -> L : TIMESTAMP    : R : p os (hd-1:tl)   
        NUMBER          -> L : NUMBER       : R : p os (hd-1:tl)   
        DIFFICULTY      -> L : DIFFICULTY   : R : p os (hd-1:tl)   
        GASLIMIT        -> L : GASLIMIT     : R : p os (hd-1:tl)   
        CHAINID         -> L : CHAINID      : R : p os (hd-1:tl)   
        SELFBALANCE     -> L : SELFBALANCE  : R : p os (hd-1:tl)   
        JUMPDEST s      -> L : JUMPDEST s   : R : p os (hd+1:tl)    
        PC              -> L : PC           : R : p os (hd-1:tl)   
        MSIZE           -> L : MSIZE        : R : p os (hd-1:tl)   
        GAS             -> L : GAS          : R : p os (hd-1:tl)   
        PUSH1  s        -> L : PUSH1  s     : R : p os (hd-1:tl)    
        PUSH2  s        -> L : PUSH2  s     : R : p os (hd-1:tl)    
        PUSH3  s        -> L : PUSH3  s     : R : p os (hd-1:tl)    
        PUSH4  s        -> L : PUSH4  s     : R : p os (hd-1:tl)    
        PUSH5  s        -> L : PUSH5  s     : R : p os (hd-1:tl)    
        PUSH6  s        -> L : PUSH6  s     : R : p os (hd-1:tl)    
        PUSH7  s        -> L : PUSH7  s     : R : p os (hd-1:tl)    
        PUSH8  s        -> L : PUSH8  s     : R : p os (hd-1:tl)    
        PUSH9  s        -> L : PUSH9  s     : R : p os (hd-1:tl)    
        PUSH10 s        -> L : PUSH10 s     : R : p os (hd-1:tl)    
        PUSH11 s        -> L : PUSH11 s     : R : p os (hd-1:tl)    
        PUSH12 s        -> L : PUSH12 s     : R : p os (hd-1:tl)    
        PUSH13 s        -> L : PUSH13 s     : R : p os (hd-1:tl)    
        PUSH14 s        -> L : PUSH14 s     : R : p os (hd-1:tl)    
        PUSH15 s        -> L : PUSH15 s     : R : p os (hd-1:tl)    
        PUSH16 s        -> L : PUSH16 s     : R : p os (hd-1:tl)    
        PUSH17 s        -> L : PUSH17 s     : R : p os (hd-1:tl)    
        PUSH18 s        -> L : PUSH18 s     : R : p os (hd-1:tl)    
        PUSH19 s        -> L : PUSH19 s     : R : p os (hd-1:tl)    
        PUSH20 s        -> L : PUSH20 s     : R : p os (hd-1:tl)    
        PUSH21 s        -> L : PUSH21 s     : R : p os (hd-1:tl)    
        PUSH22 s        -> L : PUSH22 s     : R : p os (hd-1:tl)    
        PUSH23 s        -> L : PUSH23 s     : R : p os (hd-1:tl)    
        PUSH24 s        -> L : PUSH24 s     : R : p os (hd-1:tl)    
        PUSH25 s        -> L : PUSH25 s     : R : p os (hd-1:tl)    
        PUSH26 s        -> L : PUSH26 s     : R : p os (hd-1:tl)    
        PUSH27 s        -> L : PUSH27 s     : R : p os (hd-1:tl)    
        PUSH28 s        -> L : PUSH28 s     : R : p os (hd-1:tl)    
        PUSH29 s        -> L : PUSH29 s     : R : p os (hd-1:tl)    
        PUSH30 s        -> L : PUSH30 s     : R : p os (hd-1:tl)    
        PUSH31 s        -> L : PUSH31 s     : R : p os (hd-1:tl)    
        PUSH32 s        -> L : PUSH32 s     : R : p os (hd-1:tl)    
        DUP1            -> L : DUP1         : R : p os (hd-1:tl)   
        DUP2            -> L : DUP2         : R : p os (hd-1:tl)   
        DUP3            -> L : DUP3         : R : p os (hd-1:tl)   
        DUP4            -> L : DUP4         : R : p os (hd-1:tl)   
        DUP5            -> L : DUP5         : R : p os (hd-1:tl)   
        DUP6            -> L : DUP6         : R : p os (hd-1:tl)   
        DUP7            -> L : DUP7         : R : p os (hd-1:tl)   
        DUP8            -> L : DUP8         : R : p os (hd-1:tl)   
        DUP9            -> L : DUP9         : R : p os (hd-1:tl)   
        DUP10           -> L : DUP10        : R : p os (hd-1:tl)   
        DUP11           -> L : DUP11        : R : p os (hd-1:tl)   
        DUP12           -> L : DUP12        : R : p os (hd-1:tl)   
        DUP13           -> L : DUP13        : R : p os (hd-1:tl)   
        DUP14           -> L : DUP14        : R : p os (hd-1:tl)   
        DUP15           -> L : DUP15        : R : p os (hd-1:tl)   
        DUP16           -> L : DUP16        : R : p os (hd-1:tl)   
        SWAP1           -> L : SWAP1        : R : p os (hd:tl)   
        SWAP2           -> L : SWAP2        : R : p os (hd:tl)   
        SWAP3           -> L : SWAP3        : R : p os (hd:tl)   
        SWAP4           -> L : SWAP4        : R : p os (hd:tl)   
        SWAP5           -> L : SWAP5        : R : p os (hd:tl)   
        SWAP6           -> L : SWAP6        : R : p os (hd:tl)   
        SWAP7           -> L : SWAP7        : R : p os (hd:tl)   
        SWAP8           -> L : SWAP8        : R : p os (hd:tl)   
        SWAP9           -> L : SWAP9        : R : p os (hd:tl)   
        SWAP10          -> L : SWAP10       : R : p os (hd:tl)   
        SWAP11          -> L : SWAP11       : R : p os (hd:tl)   
        SWAP12          -> L : SWAP12       : R : p os (hd:tl)   
        SWAP13          -> L : SWAP13       : R : p os (hd:tl)   
        SWAP14          -> L : SWAP14       : R : p os (hd:tl)   
        SWAP15          -> L : SWAP15       : R : p os (hd:tl)   
        SWAP16          -> L : SWAP16       : R : p os (hd:tl)   
        INVALID         -> L : INVALID      : R : p os (hd:tl)  
        INFO s          ->                        p os (hd:tl)  



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



stackArg ts = fst $ loop ts 0 where 
    loop []                     i  = ([],i)
    loop (RED STACK os: ts)     i  = (RED (ARG(i+1)) os' : ts', i'') where 
                                            (os', i')   = loop os (i+1) 
                                            (ts', i'')  = loop ts i'
    loop (RED o os: ts)         i  = (RED o os' : ts',  i'') where 
                                            (os', i')   = loop os i 
                                            (ts', i'')  = loop ts i' 
    loop (BLK o os: ts)         i  = (BLK o os' : ts' , i'') where 
                                            (os', i')   = loop os i 
                                            (ts', i'')  = loop ts i' 



knitF :: [OPCODE] -> ([RBTree OPCODE], [OPCODE]) 
knitF opcodes = case opcodes of 
    L : ops         -> let (ret,cont)   = knitT ops in 
                       let (rest,cont') = knitF cont in (ret:rest, cont') 
    R : ops         -> ([], ops) 
    e : ops         -> error (show e) 
    []              -> ([], []) 


-- combine [JUMPDEST] and the next block 
--cat         :: [RBTree OPCODE] -> [RBTree OPCODE]  
--cat []                                          = []
--cat(BLK SEQ[BLK(JUMPDEST s)[]]:BLK SEQ seq:xs)  = BLK SEQ(BLK(JUMPDEST s)[]: seq): cat xs  
--cat (x:xs)                                      = x : cat xs 
cat         :: [[RBTree OPCODE]] -> [[RBTree OPCODE]]  
cat []                                          = []
cat([BLK(JUMPDEST s)[]] : seq : xs)  = [BLK SEQ (BLK(JUMPDEST s)[] : seq)] : cat xs  
cat (x:xs)                           = x : cat xs 


--knit        :: [OPCODE] -> RBTree OPCODE 
--knit        = BLK SEQ . stackArg . rev . fst . knitF . paren   
knit        :: [OPCODE] -> [RBTree OPCODE] 
knit        = stackArg . rev . fst . knitF . paren   


knits       :: [[OPCODE]] -> [RBTree OPCODE] 
knits       = concat . cat . map knit

