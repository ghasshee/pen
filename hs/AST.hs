{-# LANGUAGE FlexibleInstances #-} 
module AST where 

import GCLL
import Type
import Term
import Tree
import Predicate 
import Datatype
    



data Decl   = FLET ID [Param] Term (Maybe Formulae)
            |  LET ID         Term (Maybe Formulae) 
            | SLET ID         Term (Maybe Formulae) 
            | DATA ID [ID] [DCONSTR] 
            deriving (Eq, Read)  

instance Show Decl where 
    show (FLET i ps t p) = "FLET " ++ i ++ " " ++ show ps ++ " := " ++ show t ++ show p ++ " IN \n" 
    show (SLET i    t p) = "SLET " ++ i ++ " "            ++ " := " ++ show t ++ show p ++ " IN \n" 
    show ( LET i    t p) = " LET " ++ i ++ " "            ++ " := " ++ show t ++ show p ++ " IN \n" 
    show (DATA i is c)   = "DATA " ++ i ++ " " ++ show is ++ " := " ++ show c           ++ " IN \n" 

data BODY   = BODY (Maybe Formulae) [Decl] Term (Maybe Formulae) 
            deriving (Eq, Read, Show) 


data TOP    = MT ID Ty [Param] BODY   
            | SV ID Ty  -- Storage Variables 
            | EV ID Ty 
            deriving (Show, Eq, Read) 


data CONTRACT 
            = CN ID [TOP] 
            deriving (Show, Eq, Read) 


instance {-# OVERLAPPING #-} Show [TOP] where 
    show []         = "\n\n"
    show (t:ts)     = "\n\n" ++ show t ++ show ts  






