module Syntax where 

import GCLL 
import Cont
import Effect

type ID     =   String

data Ty     =   TyERR 
            |   TyUnit
            |   TyU256  -- 256 bits
            |   TyU8    --   8 bits
            |   TyB32   -- 256 bits 
            |   TyADR   -- 160 bits
            |   TyBOOL
            |   TyABS   Ty Ty 
            |   TyMAP   Ty Ty 
            |   TyMTHD  ID [Ty] Ty
            |   TyDFLT
            |   TyREF   Ty
            deriving (Show, Eq, Read) 


data Top        =   CN ID [Ty] [Mthd] 
                |   EV Ty  

data Mthd       =   MT ID Ty (K Tm) 


data Tm         =   TmAPP Tm Tm             -- TmAPP (A->B) A   ::  B 
                |   TmABS ID Ty Tm          -- TmABS (A->B)     ::  A -> B 
                |   TmVAR ID 
                |   TmFIX ID ID Ty Tm       -- TmFIX (A->A)     ::  A  
                |   TmU8 Int                -- TmU8             ::  TyU8 
                |   TmU256 Integer
                |   TmI Int Int        
                |   TmIREC Int    
                |   TmISTR Int    
                |   TmIF Tm Tm Tm
                deriving (Show, Eq, Read) 



typeof (TmU256 _) = TyU256
typeof _          = undefined 


