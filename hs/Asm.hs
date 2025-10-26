module Asm where 

import Data.Char
import PreLink
import Prelude hiding (EQ,LT,GT) 

import Opcode
import Utils 
import Hex (isHex, toHex) 

import Text.Printf (printf) 




asm :: [OPCODE] -> String 
asm = map toLower . concat . map toByte 


hex :: Int -> Integer -> String
hex i n | n < 0 = error $ "hex: cannot convert negative number " ++ show n 
hex i n         = printf ( "%0" ++ show (2*i) ++ "X" ) n 

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
           PUSH0                              ->  "5F" 
           PUSH1  v                           ->  "60" ++  hex 1  v
           PUSH2  v                           ->  "61" ++  hex 2  v
           PUSH3  v                           ->  "62" ++  hex 3  v
           PUSH4  v                           ->  "63" ++  hex 4  v
           PUSH5  v                           ->  "64" ++  hex 5  v
           PUSH6  v                           ->  "65" ++  hex 6  v
           PUSH7  v                           ->  "66" ++  hex 7  v
           PUSH8  v                           ->  "67" ++  hex 8  v
           PUSH9  v                           ->  "68" ++  hex 9  v
           PUSH10 v                           ->  "69" ++  hex 10 v
           PUSH11 v                           ->  "6A" ++  hex 11 v
           PUSH12 v                           ->  "6B" ++  hex 12 v
           PUSH13 v                           ->  "6C" ++  hex 13 v
           PUSH14 v                           ->  "6D" ++  hex 14 v
           PUSH15 v                           ->  "6E" ++  hex 15 v
           PUSH16 v                           ->  "6F" ++  hex 16 v
           PUSH17 v                           ->  "70" ++  hex 17 v
           PUSH18 v                           ->  "71" ++  hex 18 v
           PUSH19 v                           ->  "72" ++  hex 19 v
           PUSH20 v                           ->  "73" ++  hex 20 v
           PUSH21 v                           ->  "74" ++  hex 21 v
           PUSH22 v                           ->  "75" ++  hex 22 v
           PUSH23 v                           ->  "76" ++  hex 23 v
           PUSH24 v                           ->  "77" ++  hex 24 v
           PUSH25 v                           ->  "78" ++  hex 25 v
           PUSH26 v                           ->  "79" ++  hex 26 v
           PUSH27 v                           ->  "7A" ++  hex 27 v
           PUSH28 v                           ->  "7B" ++  hex 28 v
           PUSH29 v                           ->  "7C" ++  hex 29 v
           PUSH30 v                           ->  "7D" ++  hex 30 v
           PUSH31 v                           ->  "7E" ++  hex 31 v
           PUSH32 v                           ->  "7F" ++  hex 32 v
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
           INFO s                             ->  hex' cs ++ bs 
                where 
                    hex'      = concat . map (toHex . toInteger . ord) 
                    (cs, bs) = span (not . isHex) s 
           e                                  ->  "??" ++ show e ++ "??"

