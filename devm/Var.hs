module Var where 


data Var        = X   Integer  
                | Arg Integer 
                deriving (Eq, Read)

instance Show Var where 
    show (X n)      = "X"   ++ show n
    show (Arg n)    = "Arg" ++ show n 



