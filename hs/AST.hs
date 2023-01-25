module AST where 

import GCLL
import Type
import Term
import Tree
import Predicate 
import Datatype
    



data Decl   = LET  ID [Param] AST (Maybe STFormulae)
            | DATA ID [ID] [DCONSTR] 
            deriving (Show, Eq, Read)  

data BODY   = BODY [Decl] AST 
            deriving (Show, Eq, Read) 

data TOP    = MT ID Ty [Param] (Maybe STFormulae) BODY (Maybe STFormulae)  
            | SV ID Ty  -- Storage Variables 
            | EV ID Ty 
            deriving (Show, Eq, Read) 

data CONTRACT 
            = CN ID [TOP] 
            deriving (Show, Eq, Read) 








