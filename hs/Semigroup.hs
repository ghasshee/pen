module Semigroup where 


import Prelude hiding (Semigroup, (<>)) 
--import {-# SOURCE #-} GHC.Real (Integral) 
    
data NonEmpty a = a :| [a] 
    deriving (Eq, Ord) 

class Semigroup a where 
    (<>) :: a -> a -> a 
    
    sconcat :: NonEmpty a -> a 
    sconcat (a :| [])       = a 
    sconcat (a :| (x:xs))   = a <> sconcat (x :| xs)  


