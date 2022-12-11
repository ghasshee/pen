module Effect where 

import Cont 
import GCLL 

-- Monad K adds effects to the Tm 
data K a        = K  a 
                | Kons STMT (K a)   deriving (Show, Eq, Read)

instance Functor K where 
    fmap        = undefined 
instance Applicative K where 
    pure        = return 
    (<*>)       = undefined 
instance Monad K where 
    return a    = K a 
    k >>= f     = case k of 
        K a       -> f a 
        Kons e k' -> Kons e (k' >>= f)
instance PCRMonad K where 
    reflect m   = C (\k -> k =<< m) 
    reify (C k) = k return 


splitK :: K a -> ([STMT], a) 
splitK (K a)        = ([], a) 
splitK (Kons s k)   = (s:ss, a) 
    where
        (ss, a) = splitK k 

bindK :: [STMT] -> a -> K a 
bindK []     a      = K a 
bindK (s:ss) a      = Kons s (bindK ss a) 

