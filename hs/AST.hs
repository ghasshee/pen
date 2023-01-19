module AST where 

import GCLL
import Type
import Term
import Tree
import Predicate 
    


type Param  = (ID,Ty) 

data Decl   = LET ID [Param] AST (Maybe STFormulae)
            deriving (Show, Eq, Read)  

data BODY   = BODY [Decl] AST 
            deriving (Show, Eq, Read) 

data TOP    = MT ID [Param] Ty (Maybe STFormulae) BODY (Maybe STFormulae)  
            | EV ID Ty 
            | SV ID Ty  -- Storage Variables 
            deriving (Show, Eq, Read) 

data CONTRACT 
            = CN ID [TOP] 
            deriving (Show, Eq, Read) 








