module Asm where 

import System.IO 
import Data.Word
import Data.Binary.Get
import Data.List 
import Data.Char


data OPCODE = UNDEFINED String
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
            | EXTCODESOPY
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
            | JUMPDEST
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

