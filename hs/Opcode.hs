module Opcode where 

import Data.Char 

import Tree 
--import Var
--import Term 
import PreLink


data OPCODE = L | R             -- Parenceses "(", ")" for Disassemling -- see Knit.hs  
            | POPFUNSTACK       -- Function Stack POP operation 
            | PUSHFUNSTACK Int  -- Function Stack PUSH operation 
            | SEQ               -- Partitioning OPCODEs for Disassembling -- see Knit.hs  
            | ARG Integer       -- ARG n is the nth element from STACK top -- see Knit.hs
            | PUSHDEST Int      -- PUSHing JUMPDEST by Virtual Node ID (Not real Jump Address) 
            | PUSH PreLinkValue -- Linker handles PreLinkValues 
            | INFO      String  -- HEX DATA Segment Contracts will have on ethereum Network
--  ^  VIRTUAL EVM OPCODES  ( for IR: Intermediate Representation ) 

--  v  REAL EVM OPCODE      ( for EVM: Ethereum Virtual Machine   )   
            | STOP
            | ADD         
            | MUL
            | SUB
            | DIV
            | SDIV
            | MOD
            | SMOD
            | ADDMOD
            | MULMOD
            | EXP
            | SIGNEXTEND
            | LT
            | GT
            | SLT
            | SGT
            | EQ
            | ISZERO
            | AND
            | OR
            | XOR
            | NOT
            | BYTE
            | SHL
            | SHR
            | SAR
            | SHA3
            | ADDRESS
            | BALANCE
            | ORIGIN
            | CALLER
            | CALLVALUE
            | CALLDATALOAD
            | CALLDATASIZE
            | CALLDATACOPY
            | CODESIZE
            | CODECOPY
            | GASPRICE
            | EXTCODESIZE
            | EXTCODECOPY
            | RETURNDATASIZE
            | RETURNDATACOPY
            | EXTCODEHASH
            | BLOCKHASH
            | COINBASE
            | TIMESTAMP
            | NUMBER
            | DIFFICULTY
            | GASLIMIT
            | CHAINID
            | SELFBALANCE
            | POP
            | MLOAD
            | MSTORE
            | MSTORE8
            | SLOAD
            | SSTORE
            | JUMP
            | JUMPI
            | PC
            | MSIZE
            | GAS
            | JUMPDEST Int
            | PUSH0 
            | PUSH1  Integer 
            | PUSH2  Integer 
            | PUSH3  Integer 
            | PUSH4  Integer 
            | PUSH5  Integer 
            | PUSH6  Integer 
            | PUSH7  Integer 
            | PUSH8  Integer 
            | PUSH9  Integer 
            | PUSH10 Integer 
            | PUSH11 Integer 
            | PUSH12 Integer 
            | PUSH13 Integer 
            | PUSH14 Integer 
            | PUSH15 Integer 
            | PUSH16 Integer 
            | PUSH17 Integer 
            | PUSH18 Integer 
            | PUSH19 Integer 
            | PUSH20 Integer 
            | PUSH21 Integer 
            | PUSH22 Integer 
            | PUSH23 Integer 
            | PUSH24 Integer 
            | PUSH25 Integer 
            | PUSH26 Integer 
            | PUSH27 Integer 
            | PUSH28 Integer 
            | PUSH29 Integer 
            | PUSH30 Integer 
            | PUSH31 Integer 
            | PUSH32 Integer 
            | DUP1 
            | DUP2 
            | DUP3 
            | DUP4 
            | DUP5 
            | DUP6 
            | DUP7 
            | DUP8 
            | DUP9 
            | DUP10
            | DUP11
            | DUP12
            | DUP13
            | DUP14
            | DUP15
            | DUP16
            | SWAP1
            | SWAP2
            | SWAP3
            | SWAP4
            | SWAP5
            | SWAP6
            | SWAP7
            | SWAP8
            | SWAP9
            | SWAP10
            | SWAP11
            | SWAP12
            | SWAP13
            | SWAP14
            | SWAP15
            | SWAP16
            | LOG0
            | LOG1
            | LOG2
            | LOG3
            | LOG4
            | CREATE
            | CALL
            | CALLCODE
            | RETURN
            | DELEGATECALL
            | CREATE2
            | STATICCALL
            | REVERT
            | INVALID
            | SELFDESTRUCT deriving (Show, Eq, Read)

{--
instance Read OPCODE where 
    readsPrec _ str = 
        case str of 
            'P':'U':'S':'H':rest -> 
                let (n, _:_:v_)    = span isDigit rest in 
                let (v,_:rest)     = span (/= '\"') v_ in 
                case (read n :: Int) of 
                    1   -> [(PUSH1  v, rest)]
                    2   -> [(PUSH2  v, rest)]
                    3   -> [(PUSH3  v, rest)]
                    4   -> [(PUSH4  v, rest)]
                    5   -> [(PUSH5  v, rest)]
                    6   -> [(PUSH6  v, rest)]
                    7   -> [(PUSH7  v, rest)]
                    8   -> [(PUSH8  v, rest)]
                    9   -> [(PUSH9  v, rest)]
                    10  -> [(PUSH10 v, rest)]
                    11  -> [(PUSH11 v, rest)]
                    12  -> [(PUSH12 v, rest)]
                    13  -> [(PUSH13 v, rest)]
            _ -> [(INVALID, str )]
--}


size :: OPCODE -> Integer 
size op = case op of 
    PUSH1  _ -> 1 + 1 
    PUSH2  _ -> 1 + 2 
    PUSH3  _ -> 1 + 3 
    PUSH4  _ -> 1 + 4 
    PUSH5  _ -> 1 + 5 
    PUSH6  _ -> 1 + 6 
    PUSH7  _ -> 1 + 7 
    PUSH8  _ -> 1 + 8  
    PUSH9  _ -> 1 + 9 
    PUSH10 _ -> 1 + 10
    PUSH11 _ -> 1 + 11
    PUSH12 _ -> 1 + 12
    PUSH13 _ -> 1 + 13
    PUSH14 _ -> 1 + 14
    PUSH15 _ -> 1 + 15
    PUSH16 _ -> 1 + 16
    PUSH17 _ -> 1 + 17
    PUSH18 _ -> 1 + 18
    PUSH19 _ -> 1 + 19
    PUSH20 _ -> 1 + 20
    PUSH21 _ -> 1 + 21
    PUSH22 _ -> 1 + 22
    PUSH23 _ -> 1 + 23
    PUSH24 _ -> 1 + 24
    PUSH25 _ -> 1 + 25
    PUSH26 _ -> 1 + 26
    PUSH27 _ -> 1 + 27
    PUSH28 _ -> 1 + 28
    PUSH29 _ -> 1 + 29
    PUSH30 _ -> 1 + 30
    PUSH31 _ -> 1 + 31
    PUSH32 _ -> 1 + 32
    _        -> 1


