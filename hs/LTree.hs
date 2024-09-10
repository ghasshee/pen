module LTree where 



import Logic 
import Set 


data LTree a    = LLf a 
                | LBr [LTree a] 

instance Show a => Show (LTree a) where 
    show (LLf a)    = show a
    show (LBr l)    = show l 

