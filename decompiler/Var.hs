module Var where 


data Var        = X Integer  
                deriving Eq

instance Show Var where 
    show (X n)  = "X" ++ show n



