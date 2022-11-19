{-# LANGUAGE FlexibleInstances #-} 

module Term where 


import Prelude hiding (EQ,GT,LT)
import Opcode
import Tree


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



data EXPR   = Ox           String
            | M              EXPR
            | S              EXPR
            | Stk         Integer
            | Var          String
            | V          EVMVALUE
            | Add       EXPR EXPR
            | Mul       EXPR EXPR 
            | Sub       EXPR EXPR
            | Div       EXPR EXPR
            | Sdiv      EXPR EXPR
            | Mod       EXPR EXPR
            | Smod      EXPR EXPR
            | Addmod    EXPR EXPR
            | Mulmod    EXPR EXPR
            | Exp       EXPR EXPR
            | Sigextend EXPR EXPR
            | Lt        EXPR EXPR
            | Gt        EXPR EXPR
            | Slt       EXPR EXPR
            | Sgt       EXPR EXPR
            | Eq        EXPR EXPR
            | Iszero         EXPR
            | And       EXPR EXPR
            | Or        EXPR EXPR
            | Xor       EXPR EXPR
            | Not            EXPR
            | Byte      EXPR EXPR
            | Shl       EXPR EXPR
            | Shr       EXPR EXPR
            | Sar       EXPR EXPR
            | Keccak         EXPR
            | Balance        EXPR
            | Extcodesize    EXPR
            | Calldataload   EXPR
            | Extcodehash    EXPR
            | Blockhash      EXPR
            | Create                        EXPR EXPR EXPR
            | Call      EXPR EXPR EXPR EXPR EXPR EXPR EXPR
            | Callcode  EXPR EXPR EXPR EXPR EXPR EXPR EXPR
            | Delegatecall   EXPR EXPR EXPR EXPR EXPR EXPR
            | Create2                  EXPR EXPR EXPR EXPR
            | Staticcall     EXPR EXPR EXPR EXPR EXPR EXPR
            deriving (Eq, Read) 

instance Show EXPR where 
    show x  = case x of 
        Ox s            -> "0x" ++ s
        M  e            -> "M[" ++ show e ++ "]"
        S  e            -> "S[" ++ show e ++ "]"
        Stk n           -> "Stk[" ++ show n ++ "]" 
        Var s           -> s  
        V v             -> show v 
        Add    y x      -> show x ++ "+" ++ show y 
        Sub    y x      -> show x ++ "-" ++ show y 
        Mul    y x      -> show x ++ "*" ++ show y 
        Div    y x      -> show x ++ "/" ++ show y 
        Sdiv   y x      -> show x ++ "/'" ++ show y   -- Sdiv : Signed Division  
        Mod    y x      -> show x ++ "%" ++ show y 
        Addmod y x      -> show x ++ "+%"++ show y 
        Mulmod y x      -> show x ++ "*%"++ show y 
        Exp    y x      -> show x ++ "^" ++ show y 
        Sigextend x y   -> "sigext " ++ show x ++ " " ++ show y
        Lt x y          -> show x ++ "<" ++ show y 
        Gt x y          -> show x ++ ">" ++ show y 
        Slt x y         -> show x ++ ">'" ++ show y 
        Sgt x y         -> show x ++ "<'" ++ show y
        Eq x y          -> show x ++ "==" ++ show y 
        Iszero x        -> "(" ++ show x ++ ")==0"
        And x y         -> show x ++ "&" ++ show y 
        Or x y          -> show x ++ "|" ++ show y
        Xor x y         -> show x ++ "(+)" ++ show y
        Not x           -> "~" ++ show x 
        Byte x n        -> show x ++ "[" ++ show n ++ "]" 
        Shl x n         -> show x ++ "<<" ++ show n 
        Shr x n         -> show x ++ ">>" ++ show n
        Sar x n         -> show x ++ ">>*" ++ show n
        Keccak x        -> "kec(" ++ show x ++ ")"
        Balance x       -> "balance(" ++ show x ++ ")"
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

optree2stmt :: RBTree OPCODE -> STMT 
optree2stmt t = loop t where 
    o2e = optree2expr
    loop t = case t of 
        BLK SEQ  []                 -> Seq [] 
        BLK SEQ  (x:xs)             -> Seq (x':xs') where 
            x'          = loop x
            Seq xs'     = loop (BLK SEQ xs) 
        BLK STOP _                  -> Stop
        BLK REVERT       [x,y]      -> Revert (o2e x) (o2e y) 
        BLK RETURN       [x,y]      -> Return (o2e x) (o2e y) 
        BLK CALLDATACOPY [x,y,z]    -> Calldatacopy (o2e x)(o2e y)(o2e z)
        BLK CODECOPY     [x,y,z]    -> Codecopy (o2e x)(o2e y)(o2e z)
        BLK EXTCODECOPY  _          -> Extcodecopy
        BLK RETURNDATACOPY _        -> undefined 
        BLK POP          _          -> Pop 
        BLK MSTORE       [a,x]      -> Assign (M(o2e x)) (o2e a)  
        BLK MSTORE8      [a,x]      -> undefined 
        BLK SSTORE       [a,x]      -> Assign (S(o2e x)) (o2e a) 
        BLK JUMP         [l]        -> Goto (o2e l) 
        BLK JUMPI        [b,l]      -> IfGoto (o2e b)(o2e l)  
        BLK (JUMPDEST s) _          -> Label s 
        BLK SWAP1        _          -> Swap 1 
        BLK SWAP2        _          -> Swap 2 
        BLK SWAP3        _          -> Swap 3 
        BLK SWAP4        _          -> Swap 4 
        BLK SWAP5        _          -> Swap 5 
        BLK SWAP6        _          -> Swap 6 
        BLK SWAP7        _          -> Swap 7 
        BLK SWAP8        _          -> Swap 8 
        BLK SWAP9        _          -> Swap 9 
        BLK SWAP10       _          -> Swap 10
        BLK SWAP11       _          -> Swap 11
        BLK SWAP12       _          -> Swap 12
        BLK SWAP13       _          -> Swap 13
        BLK SWAP14       _          -> Swap 14
        BLK SWAP15       _          -> Swap 15
        BLK SWAP16       _          -> Swap 16
        RED o            os         -> Push (o2e (RED o os))
        _          -> Seq [] 
        

optree2expr :: RBTree OPCODE -> EXPR 
optree2expr t = loop t where 
    loop t = case t of 
        RED o [] -> case o of 
            ARG    n            -> Var ("Arg[" ++ show n ++ "]")
            ADDRESS             -> V Address
            ORIGIN              -> V Origin 
            CALLER              -> V Caller 
            CALLVALUE           -> V Callvalue
            CALLDATASIZE        -> V Calldatasize
            CODESIZE            -> V Codesize
            GASPRICE            -> V Gasprice
            RETURNDATASIZE      -> V Returndatasize
            COINBASE            -> V Coinbase
            TIMESTAMP           -> V Timestamp
            NUMBER              -> V Number
            DIFFICULTY          -> V Difficulty
            CHAINID             -> V Chainid
            SELFBALANCE         -> V Selfbalance
            PC                  -> V Pc
            MSIZE               -> V Msize
            GAS                 -> V Gas
            DUP1                -> Stk 1
            DUP2                -> Stk 2
            DUP3                -> Stk 3
            DUP4                -> Stk 4
            DUP5                -> Stk 5
            DUP6                -> Stk 6
            DUP7                -> Stk 7
            DUP8                -> Stk 8
            PUSH1  s            -> Ox s 
            PUSH2  s            -> Ox s 
            PUSH3  s            -> Ox s 
            PUSH4  s            -> Ox s 
            PUSH5  s            -> Ox s 
            PUSH6  s            -> Ox s 
            PUSH7  s            -> Ox s 
            PUSH8  s            -> Ox s 
            PUSH9  s            -> Ox s 
            PUSH10 s            -> Ox s 
            PUSH11 s            -> Ox s 
            PUSH12 s            -> Ox s 
            PUSH13 s            -> Ox s 
            PUSH14 s            -> Ox s 
            PUSH15 s            -> Ox s 
            PUSH16 s            -> Ox s 
            PUSH17 s            -> Ox s 
            PUSH18 s            -> Ox s 
            PUSH19 s            -> Ox s 
            PUSH21 s            -> Ox s 
            PUSH22 s            -> Ox s 
            PUSH23 s            -> Ox s 
            PUSH24 s            -> Ox s 
            PUSH25 s            -> Ox s 
            PUSH26 s            -> Ox s 
            PUSH27 s            -> Ox s 
            PUSH28 s            -> Ox s 
            PUSH29 s            -> Ox s 
            PUSH30 s            -> Ox s 
            PUSH31 s            -> Ox s 
            PUSH32 s            -> Ox s 
            o                   -> Var ("ERR(" ++ show o ++")") 
        RED o [x]       -> case o of 
            SLOAD               -> S                (loop x) 
            MLOAD               -> M                (loop x) 
            ISZERO              -> Iszero           (loop x)
            NOT                 -> Not              (loop x)
            SHA3                -> Keccak           (loop x)
            BALANCE             -> Balance          (loop x)
            EXTCODESIZE         -> Extcodesize      (loop x)
            CALLDATALOAD        -> Calldataload     (loop x)
            EXTCODEHASH         -> Extcodehash      (loop x)
            BLOCKHASH           -> Blockhash        (loop x)
        RED o [x,y]   -> case o of 
            ADD                 -> Add      (loop x)(loop y)
            SUB                 -> Sub      (loop x)(loop y)
            MUL                 -> Mul      (loop x)(loop y)
            DIV                 -> Div      (loop x)(loop y)
            SDIV                -> Sdiv     (loop x)(loop y)
            MOD                 -> Mod      (loop x)(loop y)
            ADDMOD              -> Addmod   (loop x)(loop y)
            MULMOD              -> Mulmod   (loop x)(loop y)
            EXP                 -> Exp      (loop x)(loop y) 
            SIGNEXTEND          -> Sigextend(loop x)(loop y)
            LT                  -> Lt       (loop x)(loop y) 
            GT                  -> Gt       (loop x)(loop y) 
            SLT                 -> Slt      (loop x)(loop y) 
            SGT                 -> Sgt      (loop x)(loop y) 
            EQ                  -> Eq       (loop x)(loop y) 
            AND                 -> And      (loop x)(loop y) 
            OR                  -> Or       (loop x)(loop y) 
            XOR                 -> Xor      (loop x)(loop y) 
            BYTE                -> Byte     (loop x)(loop y) 
            SHL                 -> Shl      (loop x)(loop y) 
            SHR                 -> Shr      (loop x)(loop y) 
            SAR                 -> Sar      (loop x)(loop y) 
        RED o [x,y,z]   -> case o of 
            CREATE              -> Create   (loop x)(loop y)(loop z)
        RED o [x,y,z,w] -> case o of 
            CREATE2             -> Create2  (loop x)(loop y)(loop z)(loop w)
        RED o [a,b,c,d,e,f] -> case o of 
            DELEGATECALL        -> Delegatecall 
                                    (loop a)(loop b)(loop c)(loop d)(loop e)(loop f)
            STATICCALL          -> Staticcall   
                                    (loop a)(loop b)(loop c)(loop d)(loop e)(loop f)
        RED o [a,b,c,d,e,f,g] -> case o of 
            CALL                -> Call
                                    (loop a)(loop b)(loop c)(loop d)(loop e)(loop f)(loop g)
            CALLCODE            -> Callcode
                                    (loop a)(loop b)(loop c)(loop d)(loop e)(loop f)(loop g)






{--
data ACTION = Stop
            | Assign EXPR EXPR
            | Push EXPR
            | Cond EXPR 
            | Swap Int -- SWAP1 ( x:= S[i]; S[i]:= S[i-1] S[i-1]:=x )
            | Dup Int  -- DUP1  ( S[i+1] := S[i] ) 
            | Pop 
            | Calldatacopy  EXPR EXPR EXPR
            | Codecopy      EXPR EXPR EXPR
            | Extcodedopy
            deriving (Show, Eq, Read) 
--}

