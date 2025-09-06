module Asm where 

import Data.Char
import Prelude hiding (EQ,LT,GT) 

import Opcode
import Hex




asm :: [OPCODE] -> String 
asm = map toLower . concat . map toByte 

toByte  :: OPCODE -> String
toByte o = case o of 
           STOP                               ->  "00"
           ADD                                ->  "01"
           MUL                                ->  "02"
           SUB                                ->  "03"
           DIV                                ->  "04"
           SDIV                               ->  "05"
           MOD                                ->  "06"
           SMOD                               ->  "07"
           ADDMOD                             ->  "08"
           MULMOD                             ->  "09"
           EXP                                ->  "0A"
           SIGNEXTEND                         ->  "0B"
           LT                                 ->  "10"
           GT                                 ->  "11"
           SLT                                ->  "12"
           SGT                                ->  "13"
           EQ                                 ->  "14"
           ISZERO                             ->  "15"
           AND                                ->  "16"
           OR                                 ->  "17"
           XOR                                ->  "18"
           NOT                                ->  "19"
           BYTE                               ->  "1A"
           SHL                                ->  "1B"
           SHR                                ->  "1C"
           SAR                                ->  "1D"
           SHA3                               ->  "20"
           ADDRESS                            ->  "30"
           BALANCE                            ->  "31"
           ORIGIN                             ->  "32"
           CALLER                             ->  "33"
           CALLVALUE                          ->  "34"
           CALLDATALOAD                       ->  "35"
           CALLDATASIZE                       ->  "36"
           CALLDATACOPY                       ->  "37"
           CODESIZE                           ->  "38"
           CODECOPY                           ->  "39"
           GASPRICE                           ->  "3A"
           EXTCODESIZE                        ->  "3B"
           EXTCODECOPY                        ->  "3C"
           RETURNDATASIZE                     ->  "3D"
           RETURNDATACOPY                     ->  "3E"
           EXTCODEHASH                        ->  "3F"
           BLOCKHASH                          ->  "40"
           COINBASE                           ->  "41"
           TIMESTAMP                          ->  "42"
           NUMBER                             ->  "43"
           DIFFICULTY                         ->  "44"
           GASLIMIT                           ->  "45"
           CHAINID                            ->  "46"
           SELFBALANCE                        ->  "47"
           POP                                ->  "50"
           MLOAD                              ->  "51"
           MSTORE                             ->  "52"
           MSTORE8                            ->  "53"
           SLOAD                              ->  "54"
           SSTORE                             ->  "55"
           JUMP                               ->  "56"
           JUMPI                              ->  "57"
           PC                                 ->  "58"
           MSIZE                              ->  "59"
           GAS                                ->  "5A"
           JUMPDEST _                         ->  "5B"
           PUSH1  v                           ->  "60" ++  toHex v
           PUSH2  v                           ->  "61" ++  toHex v
           PUSH3  v                           ->  "62" ++  toHex v
           PUSH4  v                           ->  "63" ++  toHex v
           PUSH5  v                           ->  "64" ++  toHex v
           PUSH6  v                           ->  "65" ++  toHex v
           PUSH7  v                           ->  "66" ++  toHex v
           PUSH8  v                           ->  "67" ++  toHex v
           PUSH9  v                           ->  "68" ++  toHex v
           PUSH10 v                           ->  "69" ++  toHex v
           PUSH11 v                           ->  "6A" ++  toHex v
           PUSH12 v                           ->  "6B" ++  toHex v
           PUSH13 v                           ->  "6C" ++  toHex v
           PUSH14 v                           ->  "6D" ++  toHex v
           PUSH15 v                           ->  "6E" ++  toHex v
           PUSH16 v                           ->  "6F" ++  toHex v
           PUSH17 v                           ->  "70" ++  toHex v
           PUSH18 v                           ->  "71" ++  toHex v
           PUSH19 v                           ->  "72" ++  toHex v
           PUSH20 v                           ->  "73" ++  toHex v
           PUSH21 v                           ->  "74" ++  toHex v
           PUSH22 v                           ->  "75" ++  toHex v
           PUSH23 v                           ->  "76" ++  toHex v
           PUSH24 v                           ->  "77" ++  toHex v
           PUSH25 v                           ->  "78" ++  toHex v
           PUSH26 v                           ->  "79" ++  toHex v
           PUSH27 v                           ->  "7A" ++  toHex v
           PUSH28 v                           ->  "7B" ++  toHex v
           PUSH29 v                           ->  "7C" ++  toHex v
           PUSH30 v                           ->  "7D" ++  toHex v
           PUSH31 v                           ->  "7E" ++  toHex v
           PUSH32 v                           ->  "7F" ++  toHex v
           DUP1                               ->  "80"
           DUP2                               ->  "81"
           DUP3                               ->  "82"
           DUP4                               ->  "83"
           DUP5                               ->  "84"
           DUP6                               ->  "85"
           DUP7                               ->  "86"
           DUP8                               ->  "87"
           DUP9                               ->  "88"
           DUP10                              ->  "89"
           DUP11                              ->  "8A"
           DUP12                              ->  "8B"
           DUP13                              ->  "8C"
           DUP14                              ->  "8D"
           DUP15                              ->  "8E"
           DUP16                              ->  "8F"
           SWAP1                              ->  "90"
           SWAP2                              ->  "91"
           SWAP3                              ->  "92"
           SWAP4                              ->  "93"
           SWAP5                              ->  "94"
           SWAP6                              ->  "95"
           SWAP7                              ->  "96"
           SWAP8                              ->  "97"
           SWAP9                              ->  "98"
           SWAP10                             ->  "99"
           SWAP11                             ->  "9A"
           SWAP12                             ->  "9B"
           SWAP13                             ->  "9C"
           SWAP14                             ->  "9D"
           SWAP15                             ->  "9E"
           SWAP16                             ->  "9F"
           LOG0                               ->  "A0"
           LOG1                               ->  "A1"
           LOG2                               ->  "A2"
           LOG3                               ->  "A3"
           LOG4                               ->  "A4"
           CREATE                             ->  "F0"
           CALL                               ->  "F1"
           CALLCODE                           ->  "F2"
           RETURN                             ->  "F3"
           DELEGATECALL                       ->  "F4"
           CREATE2                            ->  "F5"
           STATICCALL                         ->  "FA"
           REVERT                             ->  "FD"
           INVALID                            ->  "FE"
           SELFDESTRUCT                       ->  "FF"
           INFO s                             ->  hex cs ++ bs 
                where 
                    hex      = concat . map (toHex . toInteger . ord) 
                    (cs, bs) = span (not . isHex) s 


size :: OPCODE -> Int
size o = case o of 
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







