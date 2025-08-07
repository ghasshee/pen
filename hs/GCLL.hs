{-# LANGUAGE FlexibleInstances #-} 

module GCLL where 

import Hex 
import Prelude hiding (EQ,GT,LT)

type GCLL = STMT

data EVMVALUE   = Address
                | Origin
                | Caller
                | Callvalue
                | Calldatasize
                | Codesize
                | Gasprice
                | Returndatasize
                | Coinbase
                | Timestamp
                | Number
                | Difficulty
                | Chainid
                | Selfbalance
                | Pc
                | Msize
                | Gas
                deriving (Show, Eq, Read) 





data EXPR   = Ox              Integer
            | M                  EXPR
            | S                  EXPR
            | Stk             Integer
            | Var              String
            | V              EVMVALUE
            | Add           EXPR EXPR
            | Mul           EXPR EXPR 
            | Sub           EXPR EXPR
            | Div           EXPR EXPR
            | Sdiv          EXPR EXPR
            | Mod           EXPR EXPR
            | Smod          EXPR EXPR
            | Addmod   EXPR EXPR EXPR
            | Mulmod   EXPR EXPR EXPR
            | Exp           EXPR EXPR
            | Signextend    EXPR EXPR
            | Lt            EXPR EXPR
            | Gt            EXPR EXPR
            | Slt           EXPR EXPR
            | Sgt           EXPR EXPR
            | Eq            EXPR EXPR
            | Iszero             EXPR
            | And           EXPR EXPR
            | Or            EXPR EXPR
            | Xor           EXPR EXPR
            | Not                EXPR
            | Byte          EXPR EXPR
            | Shl           EXPR EXPR
            | Shr           EXPR EXPR
            | Sar           EXPR EXPR
            | Keccak             EXPR
            | Balance            EXPR
            | Extcodesize        EXPR
            | Calldataload       EXPR
            | Extcodehash        EXPR
            | Blockhash          EXPR
            | Create   EXPR EXPR EXPR
            | Call     EXPR EXPR EXPR EXPR EXPR EXPR EXPR
            | Callcode EXPR EXPR EXPR EXPR EXPR EXPR EXPR
            | Delegatecall  EXPR EXPR EXPR EXPR EXPR EXPR
            | Create2                 EXPR EXPR EXPR EXPR
            | Staticcall    EXPR EXPR EXPR EXPR EXPR EXPR
            deriving (Eq, Read) 




instance Show EXPR where 
    show x  = case x of 
        Ox n            -> "0x" ++ toHex n
        M  e            -> "M[" ++ show e ++ "]"
        S  e            -> "S[" ++ show e ++ "]"
        Stk n           -> "Stk[" ++ show n ++ "]" 
        Var s           -> "X" ++ s  
        V v             -> show v 
        Add    y x      -> show x ++ "+" ++ show y 
        Sub    y x      -> show x ++ "-" ++ show y 
        Mul    y x      -> show x ++ "*" ++ show y 
        Div    y x      -> show x ++ "/" ++ show y 
        Sdiv   y x      -> show x ++ "/'" ++ show y   -- Sdiv : Signed Division  
        Mod    y x      -> show x ++ "%" ++ show y 
        Addmod z y x    -> "(" ++ show x ++ "+"++ show y ++ ")%" ++ show z 
        Mulmod z y x    -> "(" ++ show x ++ "*"++ show y ++ ")%" ++ show z 
        Exp    y x      -> show x ++ "^" ++ show y 
        Signextend y x  -> "sigext(" ++ show x ++ "," ++ show y ++ ")"
        Lt     y x      -> show x ++ "<" ++ show y 
        Gt     y x      -> show x ++ ">" ++ show y 
        Slt    y x      -> show x ++ ">'" ++ show y 
        Sgt    y x      -> show x ++ "<'" ++ show y
        Eq     y x      -> show x ++ "==" ++ show y 
        Iszero   x      -> "(" ++ show x ++ ")==0"
        And    y x      -> show x ++ "&" ++ show y 
        Or     y x      -> show x ++ "|" ++ show y
        Xor    y x      -> show x ++ "(+)" ++ show y
        Not      x      -> "~" ++ show x 
        Byte   x n      -> show x ++ "[" ++ show n ++ "]" 
        Shl    x n      -> show x ++ "<<" ++ show n 
        Shr    x n      -> show x ++ ">>" ++ show n
        Sar    x n      -> show x ++ ">>*" ++ show n
        Keccak   x      -> "kec(" ++ show x ++ ")"
        Balance  x      -> "balance(" ++ show x ++ ")"
        Extcodesize x   -> "extcodesize(" ++ show x ++ ")" 
        Calldataload x  -> "calldataload(" ++ show x ++ ")"
        Extcodehash x   -> "codehash(" ++ show x ++ ")"
        Blockhash x     -> "blkhash(" ++ show x ++ ")"
        Create x y z    -> "create(" ++ 
                            show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"
        Create2 x y z w         -> "create2(" ++ 
                            show x ++ ", " ++ show y ++ ", " ++ 
                            show z ++ ", " ++ show w ++ ")"
        Delegatecall a b c d e f-> "delegatecall(" ++ 
                            show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ 
                            show d ++ "," ++ show e ++ "," ++ show f ++ ")"
        Staticcall a b c d e f  -> "staticcall(" ++ 
                            show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ 
                            show d ++ "," ++ show e ++ "," ++ show f ++ ")"
        Call     a b c d e f g  -> "call(" ++ 
                             show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d ++ 
                             "," ++ show e ++ "," ++ show f ++ "," ++ show g ++ ")"
        Callcode a b c d e f g  -> "callcode(" ++ 
                            show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d ++ 
                            "," ++ show e ++ "," ++ show f ++ "," ++ show g ++ ")"

-- State Change Opcodes
--
-- Call 
-- Callcode
-- Delegatecall 
-- Staticcall
-- Create


data STMT   = Stop
            | Revert EXPR EXPR 
            | Return EXPR EXPR 
            | Label String
            | Pop 
            | Push EXPR
            | Assign EXPR EXPR
            | IfGoto  EXPR EXPR 
            | Goto EXPR 
            | Seq  [STMT] 
            | Swap Int 
            | Dup  Int 
            | Calldatacopy EXPR EXPR EXPR 
            | Codecopy     EXPR EXPR EXPR
            | Extcodecopy
            deriving (Eq, Read) 

instance Show STMT where 
    show x  = case x of 
        Stop            -> "Stop()" 
        Revert x y      -> "Revert(" ++ show x ++ ", " ++ show y ++ ")"
        Return x y      -> "Return(" ++ show x ++ ", " ++ show y ++ ")"
        Label s         -> "<< label: " ++ s ++ " >>"
        Pop             -> "Pop()"
        Push e          -> show e
        Assign x a      -> show x ++ " := " ++ show a 
        IfGoto b l      -> "If " ++ show b ++ " Goto " ++ show l 
        Goto l          -> "Goto " ++ show l 
        Seq []          -> ""
        Seq [x]         -> show x
        Seq(Label s:xs) -> show (Label s) ++ "\n" ++ show (Seq xs) 
        Seq (x:xs)      -> show x ++ ";\n" ++ show (Seq xs) 
        Swap n          -> "Swap" ++ show n 
        Dup n           -> "Dup" ++ show n 
        Calldatacopy a b c -> "Calldatacopy(" ++ 
                            show a ++ "," ++ show b ++ "," ++ show c ++ ")"
        Codecopy s f t  -> "Codecopy(to:" ++ 
                            show t ++ ",from:" ++ show f ++ ",size:" ++ show s ++ ")"
        Extcodecopy     -> "Extcodecopy"

instance {-# OVERLAPPING #-} Show [STMT] where 
    show []     = ""
    show [x]    = show x 
    show (x:xs) = show x ++ "\n" ++ show xs 



          
