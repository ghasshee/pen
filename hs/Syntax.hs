module Syntax where 

import GCLL 
--import Cont
import Effect

type ID     =   String

data Ty     =   TyERR 
            |   TyUnit
            |   TyU256  -- 256 bits
            |   TyU8    --   8 bits
            |   TyI256  -- 256 bits
            |   TyI8    --   8 bits
            |   TyB32   -- 256 bits 
            |   TyADR   -- 160 bits
            |   TyBOOL
            |   TyPROD  [Ty] 
            |   TyABS   Ty Ty 
            |   TyMAP   Ty Ty 
            |   TyMTHD  ID [Ty] Ty
            |   TyDFLT
            |   TyREF   Ty
            |   TyVAR   ID Ty 
            deriving (Show, Eq, Read) 


data Top        =   CN ID [Ty] [Mthd] 
                |   EV Ty  
                deriving (Show, Eq, Read)

data Mthd       =   MT ID Ty [Ty] (K Tm) 
                deriving (Show, Eq, Read) 

data Tm         =   TmAPP (K Tm) (K Tm)     -- TmAPP (A->B) A   ::  B 
                |   TmABS ID Ty (K Tm)      -- TmABS (A->B)     ::  A -> B 
                |   TmVAR ID 
                |   TmPROD [K Tm] 
                |   TmFIX ID ID Ty (K Tm)   -- TmFIX (A->A)     ::  A  
                |   TmU8 Int                -- TmU8             ::  TyU8 
                |   TmU256 Integer
                |   TmTRUE
                |   TmFALSE
                |   TmI Int Int        
                |   TmIREC Int    
                |   TmISTR Int    
                |   TmIF (K Tm) (K Tm) (K Tm)
                deriving (Show, Eq, Read) 


typeof (TmU256 _) = TyU256
typeof _          = undefined 


