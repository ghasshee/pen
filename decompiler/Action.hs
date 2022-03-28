module Action where 

import Prelude hiding (GT, GE) 
import Var 
import Tree 

data Action = ASSIGN Var AExpr
            | BEXPR BExpr
            | SKIP 
            deriving Eq

data BExpr  = Truth 
            | Not BExpr 
            | Eq  AExpr AExpr 
            | Gt  AExpr AExpr
            | Ge  AExpr AExpr
            | And BExpr BExpr
            | Xor BExpr BExpr
            | Or  BExpr BExpr
            -- Extensions 
            | SGt AExpr AExpr
            | Iszero AExpr
            deriving Eq

instance Show BExpr where 
    show Truth       = "⊤" 
    show (Not Truth) = "⊥" 
    show (Not b)    = "¬( " ++ show b ++ " )"
    show (Gt  a b)  = show a ++ " > " ++ show b
    show (Ge  a b)  = show a ++ " >= "++ show b  
    show (And a b)  = show a ++ " ∧ " ++ show b 
    show (Or  a b)  = show a ++ " ∨ " ++ show b 
    show (Xor a b)  = show a ++ " ⊗ " ++ show b 

data AExpr      = Num   Integer 
                | Var   Var
                | Plus  AExpr AExpr
                | Minus AExpr AExpr 
                | Times AExpr AExpr 
                | Exp   AExpr AExpr
                -- Extension 
                | Div   AExpr AExpr
                | SDiv  AExpr AExpr
                | Mod   AExpr AExpr
                | SMod  AExpr AExpr
                | SignExtend  AExpr AExpr
                | AddMod AExpr AExpr AExpr 
                | MulMod AExpr AExpr AExpr 
                | Val   EVMTerm
                | Extcodehash AExpr
                | Blockhash AExpr
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




instance Show AExpr where 
    show (Num n)    = show n 
    show (Var x)    = show x
    show (Plus a b) = "(" ++ show a ++ ")" ++ " + " ++ "(" ++ show b ++ ")"
    show (Minus a b)= "(" ++ show a ++ ")" ++ " - " ++ "(" ++ show b ++ ")"
    show (Times a b)= show a ++ " * " ++ show b
    show (Exp a b)  = show a ++ " ^ " ++ show b 


data C      = Assign Var AExpr 
            | Skip
            | Seq C C 
            | If GC 
            | Do GC 
            | Call 
            deriving Eq

data GC     = Arrow BExpr C
            | GOr GC GC 
            deriving Eq 
            



