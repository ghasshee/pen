import System.IO 
import Data.List 
import Data.Char
import Prelude hiding (EQ,LT,GT) 

import Asm
import Hex
import VM


main = do 
    words <- mkList [] 
    let prog = disasm 2 $ map (map toUpper) $ words 
    pr True 0 prog 

mkList l = do 
    e <- isEOF
    if e          then return $ reverse l else do 
    a <- getChar
    if a == '\n'  then return $ reverse l else do 
    b <- getChar 
    mkList $ [a,b] : l 


pr :: Bool -> Integer -> [OPCODE] -> IO ()
pr cr sz []       = return ()
pr cr sz (s:ss)   = do 
                    let hex = toHex sz 
                    let len = length hex
                    putStr $ replicate (8-len) '0'
                    putStr hex
                    putStr ":   "
                    print s
                    if s==INVALID 
                        then if cr 
                                then do putStrLn "// RUNTIME"; pr False 0 ss 
                                else do putStrLn "// IPFS + VERSION INFO"; pr False 0 ss
                        else pr cr (sz + size s) ss 

disasm :: Int -> [String] -> [OPCODE] 
disasm 0 l      = case l of 
    "69":"70":"66":"73":"58":"22": s -> 
        INFO "'i' 'p' 'f' 's' 58 22"        : 
        INFO (concat(take 34 s))            : disasm 0 (drop 34 s) 
    "73":"6F":"6C":"63":"43": s -> 
        INFO "'s' 'o' 'l' 'c' 43"           : 
        INFO (concat(take 3  s))            : disasm 0 (drop 3 s)
    o:s         -> INFO o                   : disasm 0 s
    []          -> []
disasm n l      = case l of 
    []          ->                                    []
    "0x":s      ->                                    disasm n s 
    "00":s      -> STOP                             : disasm n s
    "01":s      -> ADD                              : disasm n s 
    "02":s      -> MUL                              : disasm n s 
    "03":s      -> SUB                              : disasm n s
    "04":s      -> DIV                              : disasm n s 
    "05":s      -> SDIV                             : disasm n s 
    "06":s      -> MOD                              : disasm n s
    "07":s      -> SMOD                             : disasm n s 
    "08":s      -> ADDMOD                           : disasm n s
    "09":s      -> MULMOD                           : disasm n s
    "0A":s      -> EXP                              : disasm n s
    "0B":s      -> SIGNEXTEND                       : disasm n s
    "10":s      -> LT                               : disasm n s
    "11":s      -> GT                               : disasm n s
    "12":s      -> SLT                              : disasm n s
    "13":s      -> SGT                              : disasm n s
    "14":s      -> EQ                               : disasm n s
    "15":s      -> ISZERO                           : disasm n s
    "16":s      -> AND                              : disasm n s
    "17":s      -> OR                               : disasm n s
    "18":s      -> XOR                              : disasm n s
    "19":s      -> NOT                              : disasm n s
    "1A":s      -> BYTE                             : disasm n s
    "1B":s      -> SHL                              : disasm n s
    "1C":s      -> SHR                              : disasm n s
    "1D":s      -> SAR                              : disasm n s
    "20":s      -> SHA3                             : disasm n s
    "30":s      -> ADDRESS                          : disasm n s
    "31":s      -> BALANCE                          : disasm n s
    "32":s      -> ORIGIN                           : disasm n s
    "33":s      -> CALLER                           : disasm n s
    "34":s      -> CALLVALUE                        : disasm n s
    "35":s      -> CALLDATALOAD                     : disasm n s
    "36":s      -> CALLDATASIZE                     : disasm n s
    "37":s      -> CALLDATACOPY                     : disasm n s
    "38":s      -> CODESIZE                         : disasm n s
    "39":s      -> CODECOPY                         : disasm n s
    "3A":s      -> GASPRICE                         : disasm n s
    "3B":s      -> EXTCODESIZE                      : disasm n s
    "3C":s      -> EXTCODESOPY                      : disasm n s
    "3D":s      -> RETURNDATASIZE                   : disasm n s
    "3E":s      -> RETURNDATACOPY                   : disasm n s
    "3F":s      -> EXTCODEHASH                      : disasm n s
    "40":s      -> BLOCKHASH                        : disasm n s
    "41":s      -> COINBASE                         : disasm n s
    "42":s      -> TIMESTAMP                        : disasm n s
    "43":s      -> NUMBER                           : disasm n s
    "44":s      -> DIFFICULTY                       : disasm n s
    "45":s      -> GASLIMIT                         : disasm n s
    "46":s      -> CHAINID                          : disasm n s
    "47":s      -> SELFBALANCE                      : disasm n s
    "50":s      -> POP                              : disasm n s
    "51":s      -> MLOAD                            : disasm n s
    "52":s      -> MSTORE                           : disasm n s
    "53":s      -> MSTORE8                          : disasm n s
    "54":s      -> SLOAD                            : disasm n s
    "55":s      -> SSTORE                           : disasm n s
    "56":s      -> JUMP                             : disasm n s
    "57":s      -> JUMPI                            : disasm n s
    "58":s      -> PC                               : disasm n s
    "59":s      -> MSIZE                            : disasm n s
    "5A":s      -> GAS                              : disasm n s
    "5B":s      -> JUMPDEST                         : disasm n s
    "60":s      -> (PUSH1  $ concat $ take  1 s)    : disasm n (drop  1 s)
    "61":s      -> (PUSH2  $ concat $ take  2 s)    : disasm n (drop  2 s)
    "62":s      -> (PUSH3  $ concat $ take  3 s)    : disasm n (drop  3 s)
    "63":s      -> (PUSH4  $ concat $ take  4 s)    : disasm n (drop  4 s)
    "64":s      -> (PUSH5  $ concat $ take  5 s)    : disasm n (drop  5 s)
    "65":s      -> (PUSH6  $ concat $ take  6 s)    : disasm n (drop  6 s)
    "66":s      -> (PUSH7  $ concat $ take  7 s)    : disasm n (drop  7 s)
    "67":s      -> (PUSH8  $ concat $ take  8 s)    : disasm n (drop  8 s)
    "68":s      -> (PUSH9  $ concat $ take  9 s)    : disasm n (drop  9 s)
    "69":s      -> (PUSH10 $ concat $ take 10 s)    : disasm n (drop 10 s)
    "6A":s      -> (PUSH11 $ concat $ take 11 s)    : disasm n (drop 11 s)
    "6B":s      -> (PUSH12 $ concat $ take 12 s)    : disasm n (drop 12 s)
    "6C":s      -> (PUSH13 $ concat $ take 13 s)    : disasm n (drop 13 s)
    "6D":s      -> (PUSH14 $ concat $ take 14 s)    : disasm n (drop 14 s)
    "6E":s      -> (PUSH15 $ concat $ take 15 s)    : disasm n (drop 15 s)
    "6F":s      -> (PUSH16 $ concat $ take 16 s)    : disasm n (drop 16 s)
    "70":s      -> (PUSH17 $ concat $ take 17 s)    : disasm n (drop 17 s)
    "71":s      -> (PUSH18 $ concat $ take 18 s)    : disasm n (drop 18 s)
    "72":s      -> (PUSH19 $ concat $ take 19 s)    : disasm n (drop 19 s)
    "73":s      -> (PUSH20 $ concat $ take 20 s)    : disasm n (drop 20 s)
    "74":s      -> (PUSH21 $ concat $ take 21 s)    : disasm n (drop 21 s)
    "75":s      -> (PUSH22 $ concat $ take 22 s)    : disasm n (drop 22 s)
    "76":s      -> (PUSH23 $ concat $ take 23 s)    : disasm n (drop 23 s)
    "77":s      -> (PUSH24 $ concat $ take 24 s)    : disasm n (drop 24 s)
    "78":s      -> (PUSH25 $ concat $ take 25 s)    : disasm n (drop 25 s)
    "79":s      -> (PUSH26 $ concat $ take 26 s)    : disasm n (drop 26 s)
    "7A":s      -> (PUSH27 $ concat $ take 27 s)    : disasm n (drop 27 s)
    "7B":s      -> (PUSH28 $ concat $ take 28 s)    : disasm n (drop 28 s)
    "7C":s      -> (PUSH29 $ concat $ take 29 s)    : disasm n (drop 29 s)
    "7D":s      -> (PUSH30 $ concat $ take 30 s)    : disasm n (drop 30 s)
    "7E":s      -> (PUSH31 $ concat $ take 31 s)    : disasm n (drop 31 s)
    "7F":s      -> (PUSH32 $ concat $ take 32 s)    : disasm n (drop 32 s)
    "80":s      -> DUP1                             : disasm n s
    "81":s      -> DUP2                             : disasm n s
    "82":s      -> DUP3                             : disasm n s
    "83":s      -> DUP4                             : disasm n s
    "84":s      -> DUP5                             : disasm n s
    "85":s      -> DUP6                             : disasm n s
    "86":s      -> DUP7                             : disasm n s
    "87":s      -> DUP8                             : disasm n s
    "88":s      -> DUP9                             : disasm n s
    "89":s      -> DUP10                            : disasm n s
    "8A":s      -> DUP11                            : disasm n s
    "8B":s      -> DUP12                            : disasm n s
    "8C":s      -> DUP13                            : disasm n s
    "8D":s      -> DUP14                            : disasm n s
    "8E":s      -> DUP15                            : disasm n s
    "8F":s      -> DUP16                            : disasm n s
    "90":s      -> SWAP1                            : disasm n s
    "91":s      -> SWAP2                            : disasm n s
    "92":s      -> SWAP3                            : disasm n s
    "93":s      -> SWAP4                            : disasm n s
    "94":s      -> SWAP5                            : disasm n s
    "95":s      -> SWAP6                            : disasm n s
    "96":s      -> SWAP7                            : disasm n s
    "97":s      -> SWAP8                            : disasm n s
    "98":s      -> SWAP9                            : disasm n s
    "99":s      -> SWAP10                           : disasm n s
    "9A":s      -> SWAP11                           : disasm n s
    "9B":s      -> SWAP12                           : disasm n s
    "9C":s      -> SWAP13                           : disasm n s
    "9D":s      -> SWAP14                           : disasm n s
    "9E":s      -> SWAP15                           : disasm n s
    "9F":s      -> SWAP16                           : disasm n s
    "A0":s      -> LOG0                             : disasm n s
    "A1":s      -> LOG1                             : disasm n s
    "A2":s      -> LOG2                             : disasm n s
    "A3":s      -> LOG3                             : disasm n s
    "A4":s      -> LOG4                             : disasm n s
    "F0":s      -> CREATE                           : disasm n s
    "F1":s      -> CALL                             : disasm n s
    "F2":s      -> CALLCODE                         : disasm n s
    "F3":s      -> RETURN                           : disasm n s
    "F4":s      -> DELEGATECALL                     : disasm n s
    "F5":s      -> CREATE2                          : disasm n s
    "FA":s      -> STATICCALL                       : disasm n s
    "FD":s      -> REVERT                           : disasm n s
    "FE":s      -> INVALID                          : disasm (n-1) s
    "FF":s      -> SELFDESTRUCT                     : disasm n s
    e   :s      -> UNDEFINED e                      : disasm n s 

