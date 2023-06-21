module Semiring where 


import Data.Monoid 

infixl 1 <.> 
-- (<.>) :: Monoid m => m -> m -> m 
-- (<.>) = mappend 
-- 
-- one :: Monoid m => m
-- one = mempty 


class Monoid m => Semiring m where 
    zro :: m 
    (<+>) :: m -> m -> m 
    (<.>) :: m -> m -> m 
    one :: m 




data OR a       = NULL
                | LI [a]  
                | OR (OR a) (OR a)  deriving (Eq,Read) 

instance Show a => Show (OR a) where 
    show (NULL      )   = "0" 
    show (LI l      )   = show l
    show (OR a b    )   = show a ++ "  |  " ++ show b 
    

instance Semigroup (OR a) where 
    LI l <> LI m                = LI (l <> m)
    OR t s <> o                 = OR (t <> o) (s <> o) 
    o <> OR t s                 = OR (o <> t) (o <> t) 

instance Monoid (OR a) where 
    mempty                      = LI []
    mappend NULL  a             = a 
    mappend a  NULL             = a 
    mappend (LI l) (LI m)       = LI (mappend l m)  
    mappend (OR t s) o          = OR (mappend t o) (mappend s o) 
    mappend o (OR t s)          = OR (mappend o t) (mappend o s) 

instance Semiring (OR a) where 
    zro             = NULL 
    one             = mempty 
    NULL  <+> a     = a 
    a     <+> NULL  = a 
    LI [] <+> a     = a 
    a     <+> LI [] = a 
    a     <+> b     = OR a b 
    NULL  <.> a     = NULL 
    a     <.> NULL  = NULL 
    LI [] <.> a     = a 
    a     <.> LI [] = a 
    a     <.> b     = mappend a b 


sumS :: Semiring a => [a] -> a 
sumS []     = zro 
sumS (x:xs) = x <+> sumS xs 




