module Expr2Opcode where 

import GCLL 
import Opcode 
import Prelude hiding (EQ,LT,GT) 


push s      = pushN s where 
    pushN = case length s of 
        1       -> PUSH1
        2       -> PUSH1  
        4       -> PUSH2  
        6       -> PUSH3   
        8       -> PUSH4 
        10      -> PUSH5 
        12      -> PUSH6
        14      -> PUSH7 
        16      -> PUSH8 
        18      -> PUSH9 
        20      -> PUSH10 
        22      -> PUSH11 
        24      -> PUSH12 
        26      -> PUSH13 
        28      -> PUSH14 
        30      -> PUSH15
        32      -> PUSH16 
        34      -> PUSH17 
        36      -> PUSH18 
        38      -> PUSH19 
        40      -> PUSH20 
        42      -> PUSH21 
        44      -> PUSH22 
        46      -> PUSH23 
        48      -> PUSH24 
        50      -> PUSH25 
        52      -> PUSH26 
        54      -> PUSH27 
        56      -> PUSH28 
        58      -> PUSH29 
        60      -> PUSH30 
        62      -> PUSH31 
        64      -> PUSH32 
        _       -> error $ "expr2opcode.hs : pushN" ++ show s 

dup  i      = case i of 
    1       -> DUP1
    2       -> DUP2 
    3       -> DUP3 
    4       -> DUP4 
    5       -> DUP5 
    6       -> DUP6
    7       -> DUP7 
    8       -> DUP8 
    9       -> DUP9
    10      -> DUP10 
    11      -> DUP11
    12      -> DUP12 
    13      -> DUP13 
    14      -> DUP14 
    15      -> DUP15 
    16      -> DUP16 
    _       -> error "DUP n: cannot duplicate more than depth 16" 

var s   = case s of 
    ('X':n)     -> ARG $ read n 
    n           -> ARG $ read n  


value :: EVMVALUE -> OPCODE 
value  v    = case v of  
    Address         -> ADDRESS
    Origin          -> ORIGIN
    Caller          -> CALLER
    Callvalue       -> CALLVALUE
    Calldatasize    -> CALLDATASIZE
    Codesize        -> CODESIZE
    Gasprice        -> GASPRICE
    Returndatasize  -> RETURNDATASIZE
    Coinbase        -> COINBASE
    Timestamp       -> TIMESTAMP
    Number          -> NUMBER 
    Difficulty      -> DIFFICULTY
    Chainid         -> CHAINID
    Selfbalance     -> SELFBALANCE 
    Pc              -> PC
    Msize           -> MSIZE
    Gas             -> GAS 

expr2opcode :: EXPR -> [OPCODE] 
expr2opcode e = let e2o = expr2opcode in case e of 
    Ox s                -> [push s] 
    M  e                -> e2o e ++ [MLOAD] 
    S  e                -> e2o e ++ [SLOAD] 
    Stk i               -> [dup i] 
    Var s               -> [var s] 
    V evmv              -> [value evmv] 
    Add         e1 e2   -> e2o e1 ++ e2o e2 ++ [ADD] 
    Sub         e1 e2   -> e2o e1 ++ e2o e2 ++ [SUB] 
    Mul         e1 e2   -> e2o e1 ++ e2o e2 ++ [MUL] 
    Div         e1 e2   -> e2o e1 ++ e2o e2 ++ [DIV] 
    Sdiv        e1 e2   -> e2o e1 ++ e2o e2 ++ [SDIV] 
    Mod         e1 e2   -> e2o e1 ++ e2o e2 ++ [MOD] 
    Smod        e1 e2   -> e2o e1 ++ e2o e2 ++ [SMOD]
    Addmod e1   e2 e3   -> e2o e1 ++ e2o e2 ++ e2o e3 ++ [ADDMOD] 
    Mulmod e1   e2 e3   -> e2o e1 ++ e2o e2 ++ e2o e3 ++ [MULMOD] 
    Exp         e1 e2   -> e2o e1 ++ e2o e2 ++ [EXP] 
    Signextend  e1 e2   -> e2o e1 ++ e2o e2 ++ [SIGNEXTEND] 
    Lt          e1 e2   -> e2o e1 ++ e2o e2 ++ [LT] 
    Gt          e1 e2   -> e2o e1 ++ e2o e2 ++ [GT] 
    Slt         e1 e2   -> e2o e1 ++ e2o e2 ++ [SLT] 
    Sgt         e1 e2   -> e2o e1 ++ e2o e2 ++ [SGT] 
    Eq          e1 e2   -> e2o e1 ++ e2o e2 ++ [EQ] 
    Iszero e            -> e2o e  ++ [ISZERO] 
    And         e1 e2   -> e2o e1 ++ e2o e2 ++ [AND]  
    Or          e1 e2   -> e2o e1 ++ e2o e2 ++ [OR ]  
    Xor         e1 e2   -> e2o e1 ++ e2o e2 ++ [XOR]  
    Not e               -> e2o e  ++ [NOT]            
    Byte        e1 e2   -> e2o e1 ++ e2o e2 ++ [BYTE] 
    Shl         e1 e2   -> e2o e1 ++ e2o e2 ++ [SHL]  
    Shr         e1 e2   -> e2o e1 ++ e2o e2 ++ [SHR]  
    Sar         e1 e2   -> e2o e1 ++ e2o e2 ++ [SAR]  
    Keccak      e       -> e2o e  ++ [SHA3] 
    Balance     e       -> e2o e  ++ [BALANCE] 
    Extcodesize e       -> e2o e  ++ [EXTCODESIZE] 
    Calldataload e      -> e2o e  ++ [CALLDATALOAD] 
    Extcodehash  e      -> e2o e  ++ [EXTCODEHASH] 
    Blockhash    e      -> e2o e  ++ [BLOCKHASH] 
    Create e1 e2 e3     -> e2o e1 ++ e2o e2 ++ e2o e3 ++ [CREATE] 
    Call a b c d e f g  -> e2o a ++ e2o b ++ e2o c ++ e2o d ++ e2o e 
                        ++ e2o f ++ e2o g ++ [CALL] 
    Callcode a b c d e f g      -> e2o a ++ e2o b ++ e2o c ++ e2o d ++ e2o e 
                                ++ e2o f ++ e2o g ++ [CALLCODE] 
    Delegatecall a b c d e f    -> e2o a ++ e2o b ++ e2o c ++ e2o d ++ e2o e 
                                ++ e2o f ++ [DELEGATECALL] 
    Create2 a b c d     -> e2o a ++ e2o b ++ e2o c ++ e2o d ++ [CREATE2] 
    Staticcall a b c d e f  -> e2o a ++ e2o b ++ e2o c ++ e2o d ++ e2o e 
                            ++ e2o f ++ [STATICCALL] 







