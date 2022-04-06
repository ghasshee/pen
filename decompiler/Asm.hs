module Asm where 

import Tree 
import Var
import Term 

data OPCODE = L | R | SEQ 
            | STACK Integer | VAR Var 
            | EXPR Term 
            | PUSH String 
            | UNDEFINED String
            | INFO      String
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
            | JUMPDEST String
            | PUSH1  String 
            | PUSH2  String 
            | PUSH3  String 
            | PUSH4  String 
            | PUSH5  String 
            | PUSH6  String 
            | PUSH7  String 
            | PUSH8  String 
            | PUSH9  String 
            | PUSH10 String 
            | PUSH11 String 
            | PUSH12 String 
            | PUSH13 String 
            | PUSH14 String 
            | PUSH15 String 
            | PUSH16 String 
            | PUSH17 String 
            | PUSH18 String 
            | PUSH19 String 
            | PUSH20 String 
            | PUSH21 String 
            | PUSH22 String 
            | PUSH23 String 
            | PUSH24 String 
            | PUSH25 String 
            | PUSH26 String 
            | PUSH27 String 
            | PUSH28 String 
            | PUSH29 String 
            | PUSH30 String 
            | PUSH31 String 
            | PUSH32 String 
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
            | SELFDESTRUCT deriving (Show, Eq)

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


