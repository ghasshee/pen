module AST where 

import GCLL
import Type
import Term
import Tree
    

type AST    = RBTree Tm 

type Param  = (ID,Ty) 

data Decl   = LET ID [Param] AST
            deriving (Show, Eq, Read)  

data BODY   = BODY [Decl] AST 
            deriving (Show, Eq, Read) 

data Mthd   = MT ID [Param] Ty BODY 
            deriving (Show, Eq, Read) 

data Top    = CN ID [Param] [Mthd] 
            | EV ID Ty 
            deriving (Show, Eq, Read) 








