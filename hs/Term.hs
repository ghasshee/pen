module Term where 

import Tree
import Type
import GCLL 
import Data


type Bind   = (ID, Ty) 
type Param  = (ID, Ty) 

data Tm     = TmAPP                 -- 2 args 
            | TmABS ID Ty           -- 1 arg
            | TmVAR Int             -- 0
            | TmSTO Int             -- 0 Storage Variable  
            | TmPROD                -- n 
            | TmFIX ID ID Ty        -- 1
            | TmLAM ID Ty 
            | TmDATA Data
            | TmU8 Int              -- 0
            | TmU256 Integer        -- 0
            | TmTRUE                -- 0
            | TmFALSE               -- 0
            | TmNOT                 -- 1
            | TmI Int Int           -- 0 
            | TmIREC  Int           -- 0 
            | TmISTR  Int           -- 0 structural recursion 
            | TmIF                  -- 3 
            | TmAMOUNT              -- 0 EVM Value
            | TmTHIS                -- 0 THIS CONTRACT ADDRESS
            | TmSENDER              -- 0 SENDER 
            | TmCALL                -- 3 (4) args { to , value , input (, gascap) } 
            | TmRET                 
            | TmBOP String      
            | TmUOP String    
            -- Declarations 
            | TmLET  ID Ty          -- 2 assignment 
            | TmSLET ID Ty          -- 2 storage assignment 
            | TmFLET ID Ty [Param]  -- 2 Function Declaration 
            | TmMT   ID Ty [Param]  -- 2 Method   Declaration 
            | TmCN   ID 
            -- Unit Operations
            | TmSEND                -- really sending amount
            | TmTRANSFER            -- just change the balance table contracts have


            | Eff STMT
            deriving (Show, Eq, Read) 

type Term    = RBTree Tm 


show' :: Term -> String 
show' (RED (TmSTO  i)   _   )   = "S[" ++ show i ++ "]"  
show' (RED (TmVAR  i)   _   )   = "X" ++ show i  
show' (RED (TmU256 i)   _   )   = show i 
show' (RED (TmDATA d)   _   )   = show d 
show' (RED  a           []  )   = show a 
show' (RED (TmABS x ty) [a])    = "\\" ++ x ++ "." ++ show' a 
show' (RED (TmBOP o)    [a,b])  = show' a ++ o ++ show' b 
show' (RED (TmUOP o)    [a])    = o ++ show' a
show' (RED TmIF         [a,b,c])= "if" ++ show' a ++ "then" ++ show' b ++ "else" ++ show' c
show' (RED TmCALL       [a,b,c])= "call(" ++ show' a ++ ", " ++ show' b ++ ", " ++ show' c ++ ")" 
show' a                         = show a 

-- RETURN Type 
--  if you want to return Function type,
--  then you have to make a contract and embed the function into it, and 
--  returns the address of contract and the method name. 
--  
--  if the returning function was an anonymous function show the error, 
--  ERROR: RETURNING FUNCTION cannot be an ANONYMOUS FUNCTION 
--

