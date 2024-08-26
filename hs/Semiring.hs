module Semiring where 


import Data.Monoid 

infixl 6 <+> 
infixl 7 <.> 


-- (<.>) :: Monoid m => m -> m -> m 
-- (<.>) = mappend 
-- 
-- one :: Monoid m => m
-- one = mempty 



class Monoid m => Semiring m where 
    zero   :: m 
    one   :: m 
    (<+>) :: m -> m -> m 
    (<.>) :: m -> m -> m 
    srsum   :: [m] -> m 
    srprod  :: [m] -> m 
    srsum     = foldr (<+>) zero
    srprod    = foldr (<.>) one
    one     = mempty 

class Semiring a => StarSemiring a where 
    plus :: a -> a 
    star :: a -> a 
    plus a = a   <.> star a 
    star a = one <.> plus a  
    
class StarSemiring a => KleeneAlgebra a where 







    




