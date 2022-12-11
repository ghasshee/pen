module Cont where 


class Monad m => PCRMonad m where 
    reflect   :: m a -> Cont (m d) a
    reify     :: (forall d. Cont (m d) a) -> m a





newtype Cont r a = C { rC :: (a -> r) -> r }

instance Functor (Cont r) where 
    fmap        = undefined 
instance Applicative (Cont r) where 
    pure        = return 
    (<*>)       = undefined 
instance Monad (Cont r) where 
    return a    = C (\k -> k a) 
    C c >>= f   = C (\k -> c (\a -> rC (f a) k)) 
instance PCRMonad (Cont r) where 
    reflect m     = C (\k -> k =<< m)
    reify (C r)   = r return  








{--
type CPS a = forall k. (a -> k) -> k

toCPS :: a -> CPS a 

toCPS t k = flip ($) t k 

fromCPS :: CPS a -> a
fromCPS = ($ id) 

--}
