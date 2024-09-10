{-# LANGUAGE FlexibleInstances #-} 
module AST where 

import GCLL
import Type
import Term
import Tree
import Datatype
import CPS 
    



data Decl   = FLET ID [Param] Term (Maybe Formulae)
            |  LET ID         Term (Maybe Formulae) 
            | SLET ID         Term (Maybe Formulae) 
--            | DATA ID [ID] [DConstr] 
            deriving (Eq, Read)  

instance Show Decl where 
    show (FLET i ps t p) = "FLET " ++ i ++ " " ++ show ps ++ " := " ++ show t ++ show p ++ " IN \n" 
    show (SLET i    t p) = "SLET " ++ i ++ " "            ++ " := " ++ show t ++ show p ++ " IN \n" 
    show ( LET i    t p) = " LET " ++ i ++ " "            ++ " := " ++ show t ++ show p ++ " IN \n" 
--    show (DATA i is c)   = "DATA " ++ i ++ " " ++ show is ++ " := " ++ show c           ++ " IN \n" 

data BODY   = BODY (Maybe Formulae) [Decl] Term (Maybe Formulae) 
            deriving (Eq, Read, Show) 


-- LET x = t1 in t2 
-- == (ABS x t2 ) t1 
-- FLET f x = t1 in t2 
-- == LET f = FIX f x t1 in t2   
-- == (ABS f t2) (FIX f x t1) 
--SLET x = t1 in t2 
--SSTORE x t1; t2 
--
--e.g. 
-- SLET x = t1 in SLET x = x * 2 in t3 
-- == SSTORE x t1; (SLET x = (SLOAD x) * 2 in t3) 
-- == SSTORE x t1; (SSTORE x (SLOAD x * 2) ; t3 )   
--
--SLET x = 1 in LET y = x + 1 in SLET x = y * 2 in x + 3
--SSTORE x 1; (ABS y (SLET x = y * 2 in x + 3)) (SLOAD x + 1) 
--SSTORE x 1; (ABS y (SSTORE x (y * 2); SLOAD x + 3)) (SLOAD x + 1)  
--SSTORE x 1; (SSTORE x ((SLOAD x + 1) * 2); SLAOD x + 3) 
--SSTORE x 1; SSTORE x ((SLOAD x + 1) * 2); SLOAD x + 3 



data TOP    = MT ID Ty [Param] BODY     -- Method Definition 
            | MT' ID Ty [Param] Term 
            | SV ID Ty                  -- Storage Variables 
            | EV ID Ty                  -- Event Declaration 
            | DT ID [ID] [DConstr]      -- Datatype Declaration 
            deriving (Show, Eq, Read) 


data CONTRACT 
            = CN ID [TOP] 
            deriving (Show, Eq, Read) 


instance {-# OVERLAPPING #-} Show [TOP] where 
    show []         = "\n\n"
    show (t:ts)     = "\n\n" ++ show t ++ show ts  




method_call :: TOP -> CONTRACT 
method_call (MT id ty args body) = undefined 





