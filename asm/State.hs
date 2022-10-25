{-# LANGUAGE DeriveFunctor #-} 


--import Control.Monad.State 



--import Prelude hiding (fmap) 


{--
incr :: State Int () 
incr = do 
    n <- get 
    put (n+1) 
--}





data Store v k = Store (v -> k) v 

instance Functor (Store v) where 
    -- fmap :: (k -> k') -> Store v k -> Store v k' 
    fmap f (Store v k) = Store (f . v) k 


duplicate :: Store v k -> Store v (Store v k)
duplicate (Store v k) = Store (Store v) k


extend :: (Store v k -> k') -> Store v k -> Store v k'
extend f x = fmap f (duplicate x) 




type Lens k v = k -> Store v k 

get :: Lens k v -> k -> v 
get lens k = f where
    Store f x = lens k
