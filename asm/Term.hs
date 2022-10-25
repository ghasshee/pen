module Term where 


import Opcode


data EVMVALUE   = Address
                | Origin
                | Caller
                | Callvalue
                | Valldatasize
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
            deriving (Eq, Show, Read) 


{--
data STMT   = Stop
            | Pop 
            | Assign EXPR EXPR
            | If  EXPR STMT STMT  
            | Seq STMT STMT
            | Swap Int 
            | Dup  Int 
            | Calldatacopy EXPR EXPR EXPR 
            | Codecopy     EXPR EXPR EXPR
            | Extcodecopy
--}         

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



