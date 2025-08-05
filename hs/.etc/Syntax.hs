module Syntax where 

import GCLL 
--import Cont
import Effect
import Tree

type ID     =   String

data Ty     =   TyERR 
            |   TyUnit
            |   TyU256  -- 256 bits
            |   TyU8    --   8 bits
            |   TyI256  -- 256 bits
            |   TyI8    --   8 bits
            |   TyB32   -- 256 bits 
            |   TyBOOL
            |   TyPROD  [Ty] 
            |   TyABS   Ty Ty 
            |   TyMAP   Ty Ty 
            |   TyMTHD  ID [Ty] Ty
            |   TyDFLT
            |   TyREF   Ty
            |   TyVAR   ID Ty 

            |   TyADDR              -- 160 bits
            |   TyAMOUNT            -- type of Wei i.e. Ether
            |   TyINCR  Integer     -- the balance of the account is increased 
            |   TyDECR  Integer     -- the balance of the account is decreased 
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
                |   TmNOT (K Tm) 
                |   TmI Int Int        
                |   TmIREC Int    
                |   TmISTR Int    
                |   TmIF (K Tm) (K Tm) (K Tm)
                |   TmAMOUNT                -- EVM VALUE
                |   TmTHIS 
                |   TmSENDER 
                |   TmCALL 

                |   TmBOP String (K Tm) (K Tm) 
                |   TmUOP String (K Tm) 
                deriving (Show, Eq, Read) 


 -- return . KAPP :: [Km] -> K Km 
 -- KAPP       :: [Km] -> Km 
 -- KABS id ty :: [Km] -> Km 
 --
data KmNode     =   KAPP            
                |   KABS ID Ty        
                |   KVAR ID          
                |   KPROD            
                |   KFIX ID ID Ty    
                |   KU8  Int         
                |   E STMT 
                deriving (Show, Eq, Read) 

data Km         = RBTree KmNode 

-- KAPP     :: [Km] -> Km 
-- 
--eff :: ([Km] -> Km) -> [STMT] -> ([Km] -> E Km) 
--eff f ss (t:ts) = E (f ts) ss   





typeof (TmU256 _) = TyU256
typeof _          = undefined 



