import System.IO 
import Control.Monad
import Data.List 
import Data.Char
import Prelude hiding (EQ,LT,GT) 

import Asm
import Hex
import VM


main = do 
    words <- mkList [] 
    let prog = disasm 2 $ map (map toUpper) $ words 
    let line_prog = lineNo prog 
    prAsm True line_prog
    -- pr True 0 prog 
    let progs = cuts $ map snd line_prog 
    mapM print $ progs 
    let tree = fparse progs
    print tree

mkList l = do 
    e <- isEOF
    if e          then return $ reverse l else do 
    a <- getChar
    if a == '\n'  then return $ reverse l else do 
    b <- getChar 
    mkList $ [a,b] : l 

{--
pr :: Bool -> Integer -> [OPCODE] -> IO ()
pr cr sz []       = return ()
pr cr sz (o:os)   = do 
                    let hex = toHex sz 
                    let len = length hex
                    putStrLn $ replicate (8-len) '0' ++ hex ++ ":   " ++ show o
                    if o==INVALID 
                        then if cr 
                                then do putStrLn "// RUNTIME"            ; pr False 0 os 
                                else do putStrLn "// IPFS + VERSION INFO"; pr False 0 os
                        else pr cr (sz + size o) os 
--}
prAsm cr []         = return () 
prAsm cr ((n,o):os) = do
                        let hex = toHex n
                        let len = length hex 
                        putStrLn $ replicate (8-len) '0' ++ hex ++ ":   " ++ show o
                        if o == INVALID && cr       then do putStrLn "// RUNTIME";          prAsm False os else do   
                        if o == INVALID && not cr   then do putStrLn "// IPFS+VERSION INFO";prAsm False os else do
                        prAsm cr os 
    


lineNo :: [OPCODE] -> [(Integer,OPCODE)]
lineNo = loop 0 where 
    loop _ []               = [] 
    loop sz (JUMPDEST s:os) = (sz, JUMPDEST (toHex sz)) : loop (sz + 1) os
    loop sz (INVALID:os)    = (sz,INVALID)  : loop 0 os
    loop sz (o:os)          = (sz,o)        : loop (sz + size o) os

disasm :: Int -> [String] -> [OPCODE] 
disasm 0 ("69":"70":"66":"73":"58":"22": s) =  
        INFO "'i' 'p' 'f' 's' 58 22"            : 
        INFO (concat(take 34 s))                : disasm 0 (drop 34 s) 
disasm 0 ("73":"6F":"6C":"63":"43": s)      = 
        INFO "'s' 'o' 'l' 'c' 43"               : 
        INFO (concat(take 3  s))                : disasm 0 (drop 3 s)
disasm 0 (o:s)  = INFO o                        : disasm 0 s
disasm _ []     = []
disasm n(o:s)   = case o of 
    "0x"      ->                                    disasm n s 
    "00"      -> STOP                             : disasm n s
    "01"      -> ADD                              : disasm n s 
    "02"      -> MUL                              : disasm n s 
    "03"      -> SUB                              : disasm n s
    "04"      -> DIV                              : disasm n s 
    "05"      -> SDIV                             : disasm n s 
    "06"      -> MOD                              : disasm n s
    "07"      -> SMOD                             : disasm n s 
    "08"      -> ADDMOD                           : disasm n s
    "09"      -> MULMOD                           : disasm n s
    "0A"      -> EXP                              : disasm n s
    "0B"      -> SIGNEXTEND                       : disasm n s
    "10"      -> LT                               : disasm n s
    "11"      -> GT                               : disasm n s
    "12"      -> SLT                              : disasm n s
    "13"      -> SGT                              : disasm n s
    "14"      -> EQ                               : disasm n s
    "15"      -> ISZERO                           : disasm n s
    "16"      -> AND                              : disasm n s
    "17"      -> OR                               : disasm n s
    "18"      -> XOR                              : disasm n s
    "19"      -> NOT                              : disasm n s
    "1A"      -> BYTE                             : disasm n s
    "1B"      -> SHL                              : disasm n s
    "1C"      -> SHR                              : disasm n s
    "1D"      -> SAR                              : disasm n s
    "20"      -> SHA3                             : disasm n s
    "30"      -> ADDRESS                          : disasm n s
    "31"      -> BALANCE                          : disasm n s
    "32"      -> ORIGIN                           : disasm n s
    "33"      -> CALLER                           : disasm n s
    "34"      -> CALLVALUE                        : disasm n s
    "35"      -> CALLDATALOAD                     : disasm n s
    "36"      -> CALLDATASIZE                     : disasm n s
    "37"      -> CALLDATACOPY                     : disasm n s
    "38"      -> CODESIZE                         : disasm n s
    "39"      -> CODECOPY                         : disasm n s
    "3A"      -> GASPRICE                         : disasm n s
    "3B"      -> EXTCODESIZE                      : disasm n s
    "3C"      -> EXTCODECOPY                      : disasm n s
    "3D"      -> RETURNDATASIZE                   : disasm n s
    "3E"      -> RETURNDATACOPY                   : disasm n s
    "3F"      -> EXTCODEHASH                      : disasm n s
    "40"      -> BLOCKHASH                        : disasm n s
    "41"      -> COINBASE                         : disasm n s
    "42"      -> TIMESTAMP                        : disasm n s
    "43"      -> NUMBER                           : disasm n s
    "44"      -> DIFFICULTY                       : disasm n s
    "45"      -> GASLIMIT                         : disasm n s
    "46"      -> CHAINID                          : disasm n s
    "47"      -> SELFBALANCE                      : disasm n s
    "50"      -> POP                              : disasm n s
    "51"      -> MLOAD                            : disasm n s
    "52"      -> MSTORE                           : disasm n s
    "53"      -> MSTORE8                          : disasm n s
    "54"      -> SLOAD                            : disasm n s
    "55"      -> SSTORE                           : disasm n s
    "56"      -> JUMP                             : disasm n s
    "57"      -> JUMPI                            : disasm n s
    "58"      -> PC                               : disasm n s
    "59"      -> MSIZE                            : disasm n s
    "5A"      -> GAS                              : disasm n s
    "5B"      -> JUMPDEST ""                      : disasm n s
    "60"      -> PUSH1  (concat (take  1 s))      : disasm n (drop  1 s)
    "61"      -> PUSH2  (concat (take  2 s))      : disasm n (drop  2 s)
    "62"      -> PUSH3  (concat (take  3 s))      : disasm n (drop  3 s)
    "63"      -> PUSH4  (concat (take  4 s))      : disasm n (drop  4 s)
    "64"      -> PUSH5  (concat (take  5 s))      : disasm n (drop  5 s)
    "65"      -> PUSH6  (concat (take  6 s))      : disasm n (drop  6 s)
    "66"      -> PUSH7  (concat (take  7 s))      : disasm n (drop  7 s)
    "67"      -> PUSH8  (concat (take  8 s))      : disasm n (drop  8 s)
    "68"      -> PUSH9  (concat (take  9 s))      : disasm n (drop  9 s)
    "69"      -> PUSH10 (concat (take 10 s))      : disasm n (drop 10 s)
    "6A"      -> PUSH11 (concat (take 11 s))      : disasm n (drop 11 s)
    "6B"      -> PUSH12 (concat (take 12 s))      : disasm n (drop 12 s)
    "6C"      -> PUSH13 (concat (take 13 s))      : disasm n (drop 13 s)
    "6D"      -> PUSH14 (concat (take 14 s))      : disasm n (drop 14 s)
    "6E"      -> PUSH15 (concat (take 15 s))      : disasm n (drop 15 s)
    "6F"      -> PUSH16 (concat (take 16 s))      : disasm n (drop 16 s)
    "70"      -> PUSH17 (concat (take 17 s))      : disasm n (drop 17 s)
    "71"      -> PUSH18 (concat (take 18 s))      : disasm n (drop 18 s)
    "72"      -> PUSH19 (concat (take 19 s))      : disasm n (drop 19 s)
    "73"      -> PUSH20 (concat (take 20 s))      : disasm n (drop 20 s)
    "74"      -> PUSH21 (concat (take 21 s))      : disasm n (drop 21 s)
    "75"      -> PUSH22 (concat (take 22 s))      : disasm n (drop 22 s)
    "76"      -> PUSH23 (concat (take 23 s))      : disasm n (drop 23 s)
    "77"      -> PUSH24 (concat (take 24 s))      : disasm n (drop 24 s)
    "78"      -> PUSH25 (concat (take 25 s))      : disasm n (drop 25 s)
    "79"      -> PUSH26 (concat (take 26 s))      : disasm n (drop 26 s)
    "7A"      -> PUSH27 (concat (take 27 s))      : disasm n (drop 27 s)
    "7B"      -> PUSH28 (concat (take 28 s))      : disasm n (drop 28 s)
    "7C"      -> PUSH29 (concat (take 29 s))      : disasm n (drop 29 s)
    "7D"      -> PUSH30 (concat (take 30 s))      : disasm n (drop 30 s)
    "7E"      -> PUSH31 (concat (take 31 s))      : disasm n (drop 31 s)
    "7F"      -> PUSH32 (concat (take 32 s))      : disasm n (drop 32 s)
    "80"      -> DUP1                             : disasm n s
    "81"      -> DUP2                             : disasm n s
    "82"      -> DUP3                             : disasm n s
    "83"      -> DUP4                             : disasm n s
    "84"      -> DUP5                             : disasm n s
    "85"      -> DUP6                             : disasm n s
    "86"      -> DUP7                             : disasm n s
    "87"      -> DUP8                             : disasm n s
    "88"      -> DUP9                             : disasm n s
    "89"      -> DUP10                            : disasm n s
    "8A"      -> DUP11                            : disasm n s
    "8B"      -> DUP12                            : disasm n s
    "8C"      -> DUP13                            : disasm n s
    "8D"      -> DUP14                            : disasm n s
    "8E"      -> DUP15                            : disasm n s
    "8F"      -> DUP16                            : disasm n s
    "90"      -> SWAP1                            : disasm n s
    "91"      -> SWAP2                            : disasm n s
    "92"      -> SWAP3                            : disasm n s
    "93"      -> SWAP4                            : disasm n s
    "94"      -> SWAP5                            : disasm n s
    "95"      -> SWAP6                            : disasm n s
    "96"      -> SWAP7                            : disasm n s
    "97"      -> SWAP8                            : disasm n s
    "98"      -> SWAP9                            : disasm n s
    "99"      -> SWAP10                           : disasm n s
    "9A"      -> SWAP11                           : disasm n s
    "9B"      -> SWAP12                           : disasm n s
    "9C"      -> SWAP13                           : disasm n s
    "9D"      -> SWAP14                           : disasm n s
    "9E"      -> SWAP15                           : disasm n s
    "9F"      -> SWAP16                           : disasm n s
    "A0"      -> LOG0                             : disasm n s
    "A1"      -> LOG1                             : disasm n s
    "A2"      -> LOG2                             : disasm n s
    "A3"      -> LOG3                             : disasm n s
    "A4"      -> LOG4                             : disasm n s
    "F0"      -> CREATE                           : disasm n s
    "F1"      -> CALL                             : disasm n s
    "F2"      -> CALLCODE                         : disasm n s
    "F3"      -> RETURN                           : disasm n s
    "F4"      -> DELEGATECALL                     : disasm n s
    "F5"      -> CREATE2                          : disasm n s
    "FA"      -> STATICCALL                       : disasm n s
    "FD"      -> REVERT                           : disasm n s
    "FE"      -> INVALID                          : disasm (n-1) s
    "FF"      -> SELFDESTRUCT                     : disasm n s
    e         -> UNDEFINED e                      : disasm n s 

