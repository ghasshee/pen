import System.IO 
import Data.List 
import Data.Char
import Prelude hiding (EQ,LT,GT) 

import Hex
import Asm

mkList :: [String] -> IO [String]
mkList l = do 
    e <- isEOF
    if e then return $ reverse l else do 
    a <- getChar
    if a == '\n' then return $ reverse l else do 
    b <- getChar 
    let word = [a,b] 
    mkList $ word : l 

main = do 
    words <- mkList [] 
    let words' = map (map toUpper) words
    let l = disasm words'
    pr l 

pr []       = return ()
pr (s:ss)   = do putStrLn (show s); pr ss 

disasm l = case l of 
    []      -> []
    "0x":s  -> disasm s 
    "00":s  -> STOP                             : disasm s
    "01":s  -> ADD                              : disasm s 
    "02":s  -> MUL                              : disasm s 
    "03":s  -> SUB                              : disasm s
    "04":s  -> DIV                              : disasm s 
    "05":s  -> SDIV                             : disasm s 
    "06":s  -> MOD                              : disasm s
    "07":s  -> SMOD                             : disasm s 
    "08":s  -> ADDMOD                           : disasm s
    "09":s  -> MULMOD                           : disasm s
    "0A":s  -> EXP                              : disasm s
    "0B":s  -> SIGNEXTEND                       : disasm s
    "10":s  -> LT                               : disasm s
    "11":s  -> GT                               : disasm s
    "12":s  -> SLT                              : disasm s
    "13":s  -> SGT                              : disasm s
    "14":s  -> EQ                               : disasm s
    "15":s  -> ISZERO                           : disasm s
    "16":s  -> AND                              : disasm s
    "17":s  -> OR                               : disasm s
    "18":s  -> XOR                              : disasm s
    "19":s  -> NOT                              : disasm s
    "1A":s  -> BYTE                             : disasm s
    "1B":s  -> SHL                              : disasm s
    "1C":s  -> SHR                              : disasm s
    "1D":s  -> SAR                              : disasm s
    "20":s  -> SHA3                             : disasm s
    "30":s  -> ADDRESS                          : disasm s
    "31":s  -> BALANCE                          : disasm s
    "32":s  -> ORIGIN                           : disasm s
    "33":s  -> CALLER                           : disasm s
    "34":s  -> CALLVALUE                        : disasm s
    "35":s  -> CALLDATALOAD                     : disasm s
    "36":s  -> CALLDATASIZE                     : disasm s
    "37":s  -> CALLDATACOPY                     : disasm s
    "38":s  -> CODESIZE                         : disasm s
    "39":s  -> CODECOPY                         : disasm s
    "3A":s  -> GASPRICE                         : disasm s
    "3B":s  -> EXTCODESIZE                      : disasm s
    "3C":s  -> EXTCODESOPY                      : disasm s
    "3D":s  -> RETURNDATASIZE                   : disasm s
    "3E":s  -> RETURNDATACOPY                   : disasm s
    "3F":s  -> EXTCODEHASH                      : disasm s
    "40":s  -> BLOCKHASH                        : disasm s
    "41":s  -> COINBASE                         : disasm s
    "42":s  -> TIMESTAMP                        : disasm s
    "43":s  -> NUMBER                           : disasm s
    "44":s  -> DIFFICULTY                       : disasm s
    "45":s  -> GASLIMIT                         : disasm s
    "46":s  -> CHAINID                          : disasm s
    "47":s  -> SELFBALANCE                      : disasm s
    "50":s  -> POP                              : disasm s
    "51":s  -> MLOAD                            : disasm s
    "52":s  -> MSTORE                           : disasm s
    "53":s  -> MSTORE8                          : disasm s
    "54":s  -> SLOAD                            : disasm s
    "55":s  -> SSTORE                           : disasm s
    "56":s  -> JUMP                             : disasm s
    "57":s  -> JUMPI                            : disasm s
    "58":s  -> PC                               : disasm s
    "59":s  -> MSIZE                            : disasm s
    "5A":s  -> GAS                              : disasm s
    "5B":s  -> JUMPDEST                         : disasm s
    "60":s  -> (PUSH1  $ concat $ take  1 s)    : disasm (drop  1 s)
    "61":s  -> (PUSH2  $ concat $ take  2 s)    : disasm (drop  2 s)
    "62":s  -> (PUSH3  $ concat $ take  3 s)    : disasm (drop  3 s)
    "63":s  -> (PUSH4  $ concat $ take  4 s)    : disasm (drop  4 s)
    "64":s  -> (PUSH5  $ concat $ take  5 s)    : disasm (drop  5 s)
    "65":s  -> (PUSH6  $ concat $ take  6 s)    : disasm (drop  6 s)
    "66":s  -> (PUSH7  $ concat $ take  7 s)    : disasm (drop  7 s)
    "67":s  -> (PUSH8  $ concat $ take  8 s)    : disasm (drop  8 s)
    "68":s  -> (PUSH9  $ concat $ take  9 s)    : disasm (drop  9 s)
    "69":s  -> (PUSH10 $ concat $ take 10 s)    : disasm (drop 10 s)
    "6A":s  -> (PUSH11 $ concat $ take 11 s)    : disasm (drop 11 s)
    "6B":s  -> (PUSH12 $ concat $ take 12 s)    : disasm (drop 12 s)
    "6C":s  -> (PUSH13 $ concat $ take 13 s)    : disasm (drop 13 s)
    "6D":s  -> (PUSH14 $ concat $ take 14 s)    : disasm (drop 14 s)
    "6E":s  -> (PUSH15 $ concat $ take 15 s)    : disasm (drop 15 s)
    "6F":s  -> (PUSH16 $ concat $ take 16 s)    : disasm (drop 16 s)
    "70":s  -> (PUSH17 $ concat $ take 17 s)    : disasm (drop 17 s)
    "71":s  -> (PUSH18 $ concat $ take 18 s)    : disasm (drop 18 s)
    "72":s  -> (PUSH19 $ concat $ take 19 s)    : disasm (drop 19 s)
    "73":s  -> (PUSH20 $ concat $ take 20 s)    : disasm (drop 20 s)
    "74":s  -> (PUSH21 $ concat $ take 21 s)    : disasm (drop 21 s)
    "75":s  -> (PUSH22 $ concat $ take 22 s)    : disasm (drop 22 s)
    "76":s  -> (PUSH23 $ concat $ take 23 s)    : disasm (drop 23 s)
    "77":s  -> (PUSH24 $ concat $ take 24 s)    : disasm (drop 24 s)
    "78":s  -> (PUSH25 $ concat $ take 25 s)    : disasm (drop 25 s)
    "79":s  -> (PUSH26 $ concat $ take 26 s)    : disasm (drop 26 s)
    "7A":s  -> (PUSH27 $ concat $ take 27 s)    : disasm (drop 27 s)
    "7B":s  -> (PUSH28 $ concat $ take 28 s)    : disasm (drop 28 s)
    "7C":s  -> (PUSH29 $ concat $ take 29 s)    : disasm (drop 29 s)
    "7D":s  -> (PUSH30 $ concat $ take 30 s)    : disasm (drop 30 s)
    "7E":s  -> (PUSH31 $ concat $ take 31 s)    : disasm (drop 31 s)
    "7F":s  -> (PUSH32 $ concat $ take 32 s)    : disasm (drop 32 s)
    "80":s  -> DUP1                             : disasm s
    "81":s  -> DUP2                             : disasm s
    "82":s  -> DUP3                             : disasm s
    "83":s  -> DUP4                             : disasm s
    "84":s  -> DUP5                             : disasm s
    "85":s  -> DUP6                             : disasm s
    "86":s  -> DUP7                             : disasm s
    "87":s  -> DUP8                             : disasm s
    "88":s  -> DUP9                             : disasm s
    "89":s  -> DUP10                            : disasm s
    "8A":s  -> DUP11                            : disasm s
    "8B":s  -> DUP12                            : disasm s
    "8C":s  -> DUP13                            : disasm s
    "8D":s  -> DUP14                            : disasm s
    "8E":s  -> DUP15                            : disasm s
    "8F":s  -> DUP16                            : disasm s
    "90":s  -> SWAP1                            : disasm s
    "91":s  -> SWAP2                            : disasm s
    "92":s  -> SWAP3                            : disasm s
    "93":s  -> SWAP4                            : disasm s
    "94":s  -> SWAP5                            : disasm s
    "95":s  -> SWAP6                            : disasm s
    "96":s  -> SWAP7                            : disasm s
    "97":s  -> SWAP8                            : disasm s
    "98":s  -> SWAP9                            : disasm s
    "99":s  -> SWAP10                           : disasm s
    "9A":s  -> SWAP11                           : disasm s
    "9B":s  -> SWAP12                           : disasm s
    "9C":s  -> SWAP13                           : disasm s
    "9D":s  -> SWAP14                           : disasm s
    "9E":s  -> SWAP15                           : disasm s
    "9F":s  -> SWAP16                           : disasm s
    "A0":s  -> LOG0                             : disasm s
    "A1":s  -> LOG1                             : disasm s
    "A2":s  -> LOG2                             : disasm s
    "A3":s  -> LOG3                             : disasm s
    "A4":s  -> LOG4                             : disasm s
    "F0":s  -> CREATE                           : disasm s
    "F1":s  -> CALL                             : disasm s
    "F2":s  -> CALLCODE                         : disasm s
    "F3":s  -> RETURN                           : disasm s
    "F4":s  -> DELEGATECALL                     : disasm s
    "F5":s  -> CREATE2                          : disasm s
    "FA":s  -> STATICCALL                       : disasm s
    "FD":s  -> REVERT                           : disasm s
    "FE":s  -> INVALID                          : disasm s
    "FF":s  -> SELFDESTRUCT                     : disasm s
    e   :s  -> UNDEFINED (map toUpper e) : disasm s 

