module Term where 


import Mem


data Action = ASSIGN Mem Term
            | COND Term
            | SKIP 
            deriving Eq

data Term       = Num   Integer 
                | Var   Mem
                | Plus  Term Term
                | Minus Term Term  
                | Times Term Term 
                | Exp   Term Term 
                -- Extension 
                | Div   Term Term 
                | SDiv  Term Term 
                | Mod   Term Term 
                | SMod  Term Term 
                | SignExtend  Term Term 
                | AddMod Term Term Term  
                | MulMod Term Term Term  
                | Evm  EVMTerm
                | Extcodehash Term 
                | Blockhash   Term 
                -- Booleans 
                | Tru 
                | Not Term 
                | Eq  Term Term 
                | Gt  Term Term
                | Ge  Term Term 
                | And Term Term 
                | Xor Term Term 
                | Or  Term Term
                -- Extensions 
                | SGt Term Term 
                | Iszero Term 
                deriving Eq

data EVMTerm    = Address
                | Balance EVMTerm
                | Origin 
                | Caller
                | CallValue
                | Calldatasize
                | Codesize
                | Gasprice
                | Returndatasize
                | Coinbase
                | Timestamp
                | Number
                | Difficulty
                | Gaslimit
                | Chainid
                | Selfbalance
                | Msize
                | Gas
                -- 
                | Calldataload
                deriving Eq


instance Show Term where 
    show (Num n)    = show n 
    show (Var x)    = show x
    show (Plus a b) = "(" ++ show a ++ ")" ++ " + " ++ "(" ++ show b ++ ")"
    show (Minus a b)= "(" ++ show a ++ ")" ++ " - " ++ "(" ++ show b ++ ")"
    show (Times a b)= show a ++ " * " ++ show b
    show (Exp a b)  = show a ++ " ^ " ++ show b 
    show Tru        = "⊤" 
    show (Not Tru)  = "⊥" 
    show (Not b)    = "¬( " ++ show b ++ " )"
    show (Gt  a b)  = show a ++ " > " ++ show b
    show (Ge  a b)  = show a ++ " >= "++ show b  
    show (And a b)  = show a ++ " ∧ " ++ show b 
    show (Or  a b)  = show a ++ " ∨ " ++ show b 
    show (Xor a b)  = show a ++ " ⊗ " ++ show b 






data C      = Assign Mem Term 
            | Skip
            | Seq C C 
            | If GC 
            | Do GC 
            | Call 
            deriving Eq

data GC     = Arrow Term C
            | GOr GC GC 
            deriving Eq 
            



