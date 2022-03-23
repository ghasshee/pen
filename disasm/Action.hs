module Action where 

import Prelude hiding (GT, GE) 
import Var 
import Tree 

data Action = ASSIGN Var AExpr
            | BEXPR BExpr
            | SKIP 
            deriving Eq

data BExpr  = TRUE  
            | NOT BExpr 
            | EQ  AExpr AExpr 
            | GT  AExpr AExpr
            | GE  AExpr AExpr
            | AND BExpr BExpr
            | XOR BExpr BExpr
            | OR  BExpr BExpr
            deriving Eq

instance Show BExpr where 
    show TRUE       = "⊤" 
    show(NOT TRUE)  = "⊥" 
    show (NOT b)    = "¬( " ++ show b ++ " )"
    show (GT a b)   = show a ++ " > " ++ show b
    show (GE a b)   = show a ++ " >= " ++ show b  
    show (AND a b)  = show a ++ " ∧ " ++ show b 
    show (OR a b )  = show a ++ " ∨ " ++ show b 
    show (XOR a b)  = show a ++ " ⊗ " ++ show b 

data AExpr  = Num   Integer 
            | Var   Var
            | Plus  AExpr AExpr
            | Minus AExpr AExpr 
            | Times AExpr AExpr 
            | Exp   AExpr AExpr
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
            deriving Eq

data GC     = Arrow BExpr C
            | Or GC GC 
            deriving Eq 
            



