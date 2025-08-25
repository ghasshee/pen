{-# LANGUAGE FlexibleInstances #-} 

module Optree2GCLL where 


import Prelude hiding (EQ,GT,LT)

import Opcode
import GCLL 
import Tree hiding (Node) 
import Hex
import PreLink hiding (LABEL) 




optrees2stmts = map optree2stmt 

optree2stmt :: RBTree OPCODE -> STMT
optree2stmt t = loop t where 
    o2e = optree2expr
    loop t = case t of 
        BLK SEQ  []                 -> Seq [] 
        BLK SEQ  (x:xs)             -> Seq (x':xs') where 
            x'          = loop x
            Seq xs'     = loop (BLK SEQ xs) 
        BLK STOP _                  -> Stop
        BLK REVERT       [y,x]      -> Revert (o2e x) (o2e y) 
        BLK RETURN       [y,x]      -> Return (o2e x) (o2e y) 
        BLK CALLDATACOPY [z,y,x]    -> Calldatacopy (o2e x)(o2e y)(o2e z)
        BLK CODECOPY     [z,y,x]    -> Codecopy (o2e x)(o2e y)(o2e z)
        BLK EXTCODECOPY  _          -> Extcodecopy
        BLK RETURNDATACOPY [z,y,x]  -> Returndatacopy (o2e x)(o2e y)(o2e z) 
        BLK POP          _          -> Pop 
        BLK MSTORE       [a,x]      -> Assign (M(o2e x)) (o2e a)  
        BLK MSTORE8      [a,x]      -> Assign (M(o2e x)) (o2e a)
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
            PUSH (FUN v)        -> Ox $ toInteger v 
            PUSHDEST i          -> LABEL i
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
            PUSH0               -> Ox 0 
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
            PUSH20 s            -> Ox s 
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
        RED o [y,x]   -> case o of 
            ADD                 -> Add       (loop x)(loop y)
            SUB                 -> Sub       (loop x)(loop y)
            MUL                 -> Mul       (loop x)(loop y)
            DIV                 -> Div       (loop x)(loop y)
            SDIV                -> Sdiv      (loop x)(loop y)
            MOD                 -> Mod       (loop x)(loop y)
            EXP                 -> Exp       (loop x)(loop y) 
            SIGNEXTEND          -> Signextend(loop x)(loop y)
            LT                  -> Lt        (loop x)(loop y) 
            GT                  -> Gt        (loop x)(loop y) 
            SLT                 -> Slt       (loop x)(loop y) 
            SGT                 -> Sgt       (loop x)(loop y) 
            EQ                  -> Eq        (loop x)(loop y) 
            AND                 -> And       (loop x)(loop y) 
            OR                  -> Or        (loop x)(loop y) 
            XOR                 -> Xor       (loop x)(loop y) 
            BYTE                -> Byte      (loop x)(loop y) 
            SHL                 -> Shl       (loop x)(loop y) 
            SHR                 -> Shr       (loop x)(loop y) 
            SAR                 -> Sar       (loop x)(loop y) 
        RED o [z,y,x]   -> case o of 
            ADDMOD              -> Addmod   (loop x)(loop y)(loop z)
            MULMOD              -> Mulmod   (loop x)(loop y)(loop z)
            CREATE              -> Create   (loop x)(loop y)(loop z)
            o                   -> error $ "optree2expr: 3 arg pattern :: " ++ show o ++ " not found" 
        RED o [w,z,y,x] -> case o of 
            CREATE2             -> Create2  (loop x)(loop y)(loop z)(loop w)
        RED o [f,e,d,c,b,a] -> case o of 
            DELEGATECALL        -> Delegatecall 
                                    (loop a)(loop b)(loop c)(loop d)(loop e)(loop f)
            STATICCALL          -> Staticcall   
                                    (loop a)(loop b)(loop c)(loop d)(loop e)(loop f)
        RED o [g,f,e,d,c,b,a] -> case o of 
            CALL                -> Call
                                    (loop a)(loop b)(loop c)(loop d)(loop e)(loop f)(loop g)
            CALLCODE            -> Callcode
                                    (loop a)(loop b)(loop c)(loop d)(loop e)(loop f)(loop g)
        tr                      -> error $ "optree2expr:: " ++ show tr  






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

