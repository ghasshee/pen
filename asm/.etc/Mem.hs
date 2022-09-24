module Mem where 



data Mem    = M Integer 
            | S Integer 
            | X Integer  
            deriving Eq 


instance Show Mem where 
    show (M n) = "M[" ++ show n ++ "]" 
    show (S n) = "S[" ++ show n ++ "]" 
    show (X n) = "X"  ++ show n


