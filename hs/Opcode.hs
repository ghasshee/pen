module Opcode where 

import Data.Char 

import Tree 
--import Var
--import Term 
import PreLink
import Utils
import Prelude hiding (EQ,LT,GT)


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




pushdatasize :: Integer  -> Int 
pushdatasize 0  = 0 
pushdatasize i  = 1 + pushdatasize (i `div` 0x100) 

pushsize :: Integer -> Int 
pushsize i = 1 + pushdatasize i 

size :: OPCODE -> Int
size o = case o of 
           PUSH (INT i)                       ->  pushsize i 
           PUSH (FUN i)                       ->  pushsize (to i) 
           PUSH RN_SIZE                       ->  9 
           PUSH RN_OFFSET                     ->  3 
           PUSHDEST _                         ->  9
           STOP                               ->  1
           ADD                                ->  1
           MUL                                ->  1
           SUB                                ->  1
           DIV                                ->  1
           SDIV                               ->  1
           MOD                                ->  1
           SMOD                               ->  1
           ADDMOD                             ->  1
           MULMOD                             ->  1
           EXP                                ->  1
           SIGNEXTEND                         ->  1
           LT                                 ->  1
           GT                                 ->  1
           SLT                                ->  1
           SGT                                ->  1
           EQ                                 ->  1
           ISZERO                             ->  1
           AND                                ->  1
           OR                                 ->  1
           XOR                                ->  1
           NOT                                ->  1
           BYTE                               ->  1
           SHL                                ->  1
           SHR                                ->  1
           SAR                                ->  1
           SHA3                               ->  1
           ADDRESS                            ->  1
           BALANCE                            ->  1
           ORIGIN                             ->  1
           CALLER                             ->  1
           CALLVALUE                          ->  1
           CALLDATALOAD                       ->  1
           CALLDATASIZE                       ->  1
           CALLDATACOPY                       ->  1
           CODESIZE                           ->  1
           CODECOPY                           ->  1
           GASPRICE                           ->  1
           EXTCODESIZE                        ->  1
           EXTCODECOPY                        ->  1
           RETURNDATASIZE                     ->  1
           RETURNDATACOPY                     ->  1
           EXTCODEHASH                        ->  1
           BLOCKHASH                          ->  1
           COINBASE                           ->  1
           TIMESTAMP                          ->  1
           NUMBER                             ->  1
           DIFFICULTY                         ->  1
           GASLIMIT                           ->  1
           CHAINID                            ->  1
           SELFBALANCE                        ->  1
           POP                                ->  1
           MLOAD                              ->  1
           MSTORE                             ->  1
           MSTORE8                            ->  1
           SLOAD                              ->  1
           SSTORE                             ->  1
           JUMP                               ->  1
           JUMPI                              ->  1
           PC                                 ->  1
           MSIZE                              ->  1
           GAS                                ->  1
           JUMPDEST _                         ->  1
           PUSH0                              ->  1 
           PUSH1  v                           ->  2
           PUSH2  v                           ->  3
           PUSH3  v                           ->  4
           PUSH4  v                           ->  5
           PUSH5  v                           ->  6
           PUSH6  v                           ->  7
           PUSH7  v                           ->  8
           PUSH8  v                           ->  9
           PUSH9  v                           ->  10
           PUSH10 v                           ->  11
           PUSH11 v                           ->  12
           PUSH12 v                           ->  13
           PUSH13 v                           ->  14
           PUSH14 v                           ->  15
           PUSH15 v                           ->  16
           PUSH16 v                           ->  17
           PUSH17 v                           ->  18
           PUSH18 v                           ->  19
           PUSH19 v                           ->  20
           PUSH20 v                           ->  21
           PUSH21 v                           ->  22
           PUSH22 v                           ->  23
           PUSH23 v                           ->  24
           PUSH24 v                           ->  25
           PUSH25 v                           ->  26
           PUSH26 v                           ->  27
           PUSH27 v                           ->  28
           PUSH28 v                           ->  29
           PUSH29 v                           ->  30
           PUSH30 v                           ->  31
           PUSH31 v                           ->  32
           PUSH32 v                           ->  33
           DUP1                               ->  1
           DUP2                               ->  1
           DUP3                               ->  1
           DUP4                               ->  1
           DUP5                               ->  1
           DUP6                               ->  1
           DUP7                               ->  1
           DUP8                               ->  1
           DUP9                               ->  1
           DUP10                              ->  1
           DUP11                              ->  1
           DUP12                              ->  1
           DUP13                              ->  1
           DUP14                              ->  1
           DUP15                              ->  1
           DUP16                              ->  1
           SWAP1                              ->  1
           SWAP2                              ->  1
           SWAP3                              ->  1
           SWAP4                              ->  1
           SWAP5                              ->  1
           SWAP6                              ->  1
           SWAP7                              ->  1
           SWAP8                              ->  1
           SWAP9                              ->  1
           SWAP10                             ->  1
           SWAP11                             ->  1
           SWAP12                             ->  1
           SWAP13                             ->  1
           SWAP14                             ->  1
           SWAP15                             ->  1
           SWAP16                             ->  1
           LOG0                               ->  1
           LOG1                               ->  1
           LOG2                               ->  1
           LOG3                               ->  1
           LOG4                               ->  1
           CREATE                             ->  1
           CALL                               ->  1
           CALLCODE                           ->  1
           RETURN                             ->  1
           DELEGATECALL                       ->  1
           CREATE2                            ->  1
           STATICCALL                         ->  1
           REVERT                             ->  1
           INVALID                            ->  1
           SELFDESTRUCT                       ->  1
           INFO s                             ->  (length s) * 2 
           e                                  -> error $ "size: undefined on " ++ show e  








