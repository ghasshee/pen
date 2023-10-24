module Semiring where 


import Data.Monoid 

infixl 1 <.> 
-- (<.>) :: Monoid m => m -> m -> m 
-- (<.>) = mappend 
-- 
-- one :: Monoid m => m
-- one = mempty 


class Monoid m => Semiring m where 
    zro   :: m 
    one   :: m 
    (<+>) :: m -> m -> m 
    (<.>) :: m -> m -> m 



sumS :: Semiring a => [a] -> a 
sumS []     = zro 
sumS (x:xs) = x <+> sumS xs 




