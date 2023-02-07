module Term where 

import Tree
import Type
import GCLL 



type Bind   = (ID, Ty) 
type Param  = (ID, Ty) 

data Tm     = TmAPP                 -- 2 args 
            | TmABS ID Ty           -- 1 arg
            | TmVAR Int             -- 0
            | TmSTO Int             -- 0 Storage Variable  
            | TmPROD                -- n 
            | TmFIX ID ID Ty        -- 1
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
            | TmSEND                -- 


            | Eff STMT
            deriving (Show, Eq, Read) 

type AST    = RBTree Tm 



-- RETURN Type 
--  if you want to return Function type,
--  then you have to make a contract and embed the function into it, and 
--  returns the address of contract and the method name. 
--  
--  if the returning function was an anonymous function show the error, 
--  ERROR: RETURNING FUNCTION cannot be an ANONYMOUS FUNCTION 
--

