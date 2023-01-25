module Datatype where 

import Type 

data DCONSTR    = DCONSTR String [Ty] 
                deriving (Show, Eq, Read) 
