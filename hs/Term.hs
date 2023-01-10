module Term where 

import Type
import GCLL 

data Tm     = TmAPP             -- 2 args 
            | TmABS ID Ty       -- 1 arg
            | TmVAR ID          -- 0
            | TmPROD            -- n 
            | TmFIX ID ID Ty    -- 1
            | TmU8 Int          -- 0
            | TmU256 Integer    -- 0
            | TmTRUE            -- 0
            | TmFALSE           -- 0
            | TmNOT             -- 1
            | TmI Int Int       -- 0 
            | TmIREC  Int       -- 0 
            | TmISTR  Int       -- 0 structural recursion 
            | TmIF              -- 3 
            | TmAMOUNT          -- 0 EVM Value
            | TmTHIS            -- 0 THIS CONTRACT ADDRESS
            | TmSENDER          -- 0 SENDER 
            | TmCALL            -- 3 (4) args { to , value , input (, gascap) } 
            | TmBOP String      
            | TmUOP String    

            | E STMT
            deriving (Show, Eq, Read) 




