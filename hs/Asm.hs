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

