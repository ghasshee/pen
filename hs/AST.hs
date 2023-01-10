module AST where 

import GCLL
import Type
import Term
import Tree
    

data AST    = RBTree Tm 

type Param  = (ID,Ty) 

data Decl   = LET ID [Param] AST

data BODY   = BODY [Decl] AST 
            deriving (Show, Eq, Read) 

data Mthd   = MT ID Ty BODY 
            deriving (Show, Eq, Read) 

data Top    = CN ID [Ty] [Mthd] 
            | EV ID Ty 
            deriving (Show, Eq, Read) 








