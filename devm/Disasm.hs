module Disasm where 

import Control.Monad
import Prelude hiding (EQ,LT,GT) 

import Opcode
import Hex

extract     = map snd
len         = length 
rep         = replicate 
zeropad n   = rep n '0'
pr          = putStrLn 


prAsm :: [(Integer,OPCODE)] -> IO ()
prAsm = loop True where 
    loop cr []         = return () 
    loop cr ((n,o):os) = do
        let hex = toHex n
        pr $ zeropad (8-len hex) ++ hex ++ ":   " ++ show o
        if o==INVALID&&cr     then do pr "// RUNTIME"   ; loop False os else do   
            if o==INVALID&&not cr then do pr "// IPFS/VER"  ; loop False os else do
                loop cr os 


lineNo :: [OPCODE] -> [(Integer,OPCODE)]
lineNo = loop 0 where 
    loop _ []               = [] 
    loop sz (JUMPDEST s:os) = (sz,JUMPDEST(toHex sz))   : loop (sz+1) os
    loop sz (INVALID:os)    = (sz,INVALID)              : loop 0 os
    loop sz (o:os)          = (sz,o)                    : loop (sz+size o) os


disAsm :: [String] -> [OPCODE] 
disAsm = loop 2 where 
    loop _ []       =   []
    loop 0 (" ":s)  = INVALID : loop 2 s 
    loop 0 ("69":"70":"66":"73":"58":"22": s) =  
                        INFO "ipfs5822"     : 
                        INFO (concat(take 34 s))         : loop 0 (drop 34 s) 
    loop 0 ("73":"6F":"6C":"63":"43": s)      = 
                        INFO "solc43"        : 
                        INFO (concat(take 3  s))         : loop 0 (drop 3 s)
    loop 0 (o:s)    =   INFO o                           : loop 0 s 
    loop n (o:s)    =   case o of 
        "0X"        ->                                     loop n s 
        "00"        ->  STOP                             : loop n s
        "01"        ->  ADD                              : loop n s 
        "02"        ->  MUL                              : loop n s 
        "03"        ->  SUB                              : loop n s
        "04"        ->  DIV                              : loop n s 
        "05"        ->  SDIV                             : loop n s 
        "06"        ->  MOD                              : loop n s
        "07"        ->  SMOD                             : loop n s 
        "08"        ->  ADDMOD                           : loop n s
        "09"        ->  MULMOD                           : loop n s
        "0A"        ->  EXP                              : loop n s
        "0B"        ->  SIGNEXTEND                       : loop n s
        "10"        ->  LT                               : loop n s
        "11"        ->  GT                               : loop n s
        "12"        ->  SLT                              : loop n s
        "13"        ->  SGT                              : loop n s
        "14"        ->  EQ                               : loop n s
        "15"        ->  ISZERO                           : loop n s
        "16"        ->  AND                              : loop n s
        "17"        ->  OR                               : loop n s
        "18"        ->  XOR                              : loop n s
        "19"        ->  NOT                              : loop n s
        "1A"        ->  BYTE                             : loop n s
        "1B"        ->  SHL                              : loop n s
        "1C"        ->  SHR                              : loop n s
        "1D"        ->  SAR                              : loop n s
        "20"        ->  SHA3                             : loop n s
        "30"        ->  ADDRESS                          : loop n s
        "31"        ->  BALANCE                          : loop n s
        "32"        ->  ORIGIN                           : loop n s
        "33"        ->  CALLER                           : loop n s
        "34"        ->  CALLVALUE                        : loop n s
        "35"        ->  CALLDATALOAD                     : loop n s
        "36"        ->  CALLDATASIZE                     : loop n s
        "37"        ->  CALLDATACOPY                     : loop n s
        "38"        ->  CODESIZE                         : loop n s
        "39"        ->  CODECOPY                         : loop n s
        "3A"        ->  GASPRICE                         : loop n s
        "3B"        ->  EXTCODESIZE                      : loop n s
        "3C"        ->  EXTCODECOPY                      : loop n s
        "3D"        ->  RETURNDATASIZE                   : loop n s
        "3E"        ->  RETURNDATACOPY                   : loop n s
        "3F"        ->  EXTCODEHASH                      : loop n s
        "40"        ->  BLOCKHASH                        : loop n s
        "41"        ->  COINBASE                         : loop n s
        "42"        ->  TIMESTAMP                        : loop n s
        "43"        ->  NUMBER                           : loop n s
        "44"        ->  DIFFICULTY                       : loop n s
        "45"        ->  GASLIMIT                         : loop n s
        "46"        ->  CHAINID                          : loop n s
        "47"        ->  SELFBALANCE                      : loop n s
        "50"        ->  POP                              : loop n s
        "51"        ->  MLOAD                            : loop n s
        "52"        ->  MSTORE                           : loop n s
        "53"        ->  MSTORE8                          : loop n s
        "54"        ->  SLOAD                            : loop n s
        "55"        ->  SSTORE                           : loop n s
        "56"        ->  JUMP                             : loop n s
        "57"        ->  JUMPI                            : loop n s
        "58"        ->  PC                               : loop n s
        "59"        ->  MSIZE                            : loop n s
        "5A"        ->  GAS                              : loop n s
        "5B"        ->  JUMPDEST ""                      : loop n s
        "5F"        ->  PUSH0                            : loop n s 
        "60"        ->  PUSH1  (concat (take  1 s))      : loop n (drop  1 s)
        "61"        ->  PUSH2  (concat (take  2 s))      : loop n (drop  2 s)
        "62"        ->  PUSH3  (concat (take  3 s))      : loop n (drop  3 s)
        "63"        ->  PUSH4  (concat (take  4 s))      : loop n (drop  4 s)
        "64"        ->  PUSH5  (concat (take  5 s))      : loop n (drop  5 s)
        "65"        ->  PUSH6  (concat (take  6 s))      : loop n (drop  6 s)
        "66"        ->  PUSH7  (concat (take  7 s))      : loop n (drop  7 s)
        "67"        ->  PUSH8  (concat (take  8 s))      : loop n (drop  8 s)
        "68"        ->  PUSH9  (concat (take  9 s))      : loop n (drop  9 s)
        "69"        ->  PUSH10 (concat (take 10 s))      : loop n (drop 10 s)
        "6A"        ->  PUSH11 (concat (take 11 s))      : loop n (drop 11 s)
        "6B"        ->  PUSH12 (concat (take 12 s))      : loop n (drop 12 s)
        "6C"        ->  PUSH13 (concat (take 13 s))      : loop n (drop 13 s)
        "6D"        ->  PUSH14 (concat (take 14 s))      : loop n (drop 14 s)
        "6E"        ->  PUSH15 (concat (take 15 s))      : loop n (drop 15 s)
        "6F"        ->  PUSH16 (concat (take 16 s))      : loop n (drop 16 s)
        "70"        ->  PUSH17 (concat (take 17 s))      : loop n (drop 17 s)
        "71"        ->  PUSH18 (concat (take 18 s))      : loop n (drop 18 s)
        "72"        ->  PUSH19 (concat (take 19 s))      : loop n (drop 19 s)
        "73"        ->  PUSH20 (concat (take 20 s))      : loop n (drop 20 s)
        "74"        ->  PUSH21 (concat (take 21 s))      : loop n (drop 21 s)
        "75"        ->  PUSH22 (concat (take 22 s))      : loop n (drop 22 s)
        "76"        ->  PUSH23 (concat (take 23 s))      : loop n (drop 23 s)
        "77"        ->  PUSH24 (concat (take 24 s))      : loop n (drop 24 s)
        "78"        ->  PUSH25 (concat (take 25 s))      : loop n (drop 25 s)
        "79"        ->  PUSH26 (concat (take 26 s))      : loop n (drop 26 s)
        "7A"        ->  PUSH27 (concat (take 27 s))      : loop n (drop 27 s)
        "7B"        ->  PUSH28 (concat (take 28 s))      : loop n (drop 28 s)
        "7C"        ->  PUSH29 (concat (take 29 s))      : loop n (drop 29 s)
        "7D"        ->  PUSH30 (concat (take 30 s))      : loop n (drop 30 s)
        "7E"        ->  PUSH31 (concat (take 31 s))      : loop n (drop 31 s)
        "7F"        ->  PUSH32 (concat (take 32 s))      : loop n (drop 32 s)
        "80"        ->  DUP1                             : loop n s
        "81"        ->  DUP2                             : loop n s
        "82"        ->  DUP3                             : loop n s
        "83"        ->  DUP4                             : loop n s
        "84"        ->  DUP5                             : loop n s
        "85"        ->  DUP6                             : loop n s
        "86"        ->  DUP7                             : loop n s
        "87"        ->  DUP8                             : loop n s
        "88"        ->  DUP9                             : loop n s
        "89"        ->  DUP10                            : loop n s
        "8A"        ->  DUP11                            : loop n s
        "8B"        ->  DUP12                            : loop n s
        "8C"        ->  DUP13                            : loop n s
        "8D"        ->  DUP14                            : loop n s
        "8E"        ->  DUP15                            : loop n s
        "8F"        ->  DUP16                            : loop n s
        "90"        ->  SWAP1                            : loop n s
        "91"        ->  SWAP2                            : loop n s
        "92"        ->  SWAP3                            : loop n s
        "93"        ->  SWAP4                            : loop n s
        "94"        ->  SWAP5                            : loop n s
        "95"        ->  SWAP6                            : loop n s
        "96"        ->  SWAP7                            : loop n s
        "97"        ->  SWAP8                            : loop n s
        "98"        ->  SWAP9                            : loop n s
        "99"        ->  SWAP10                           : loop n s
        "9A"        ->  SWAP11                           : loop n s
        "9B"        ->  SWAP12                           : loop n s
        "9C"        ->  SWAP13                           : loop n s
        "9D"        ->  SWAP14                           : loop n s
        "9E"        ->  SWAP15                           : loop n s
        "9F"        ->  SWAP16                           : loop n s
        "A0"        ->  LOG0                             : loop n s
        "A1"        ->  LOG1                             : loop n s
        "A2"        ->  LOG2                             : loop n s
        "A3"        ->  LOG3                             : loop n s
        "A4"        ->  LOG4                             : loop n s
        "F0"        ->  CREATE                           : loop n s
        "F1"        ->  CALL                             : loop n s
        "F2"        ->  CALLCODE                         : loop n s
        "F3"        ->  RETURN                           : loop n s
        "F4"        ->  DELEGATECALL                     : loop n s
        "F5"        ->  CREATE2                          : loop n s
        "FA"        ->  STATICCALL                       : loop n s
        "FD"        ->  REVERT                           : loop n s
        "FE"        ->  INVALID                          : loop (n-1) s
        "FF"        ->  SELFDESTRUCT                     : loop n s

