{-# OPTIONS_GHC -O0 -fmax-simplifier-iterations=0 #-} 

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Cont 
import System.IO.Unsafe
import Data.Function (fix)


--foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
{--
foldrM f z xs = runCont (go xs) return where 
    go []       = ContT $ \k -> k z
    go (x:xs)   = ContT $ \k -> go xs `runContT` \r -> f x r >>= k 
--}

foldrM' f z xs = foldr step return xs z where 
    step x k = \acc -> f x acc >>= k

foldrM'' f z xs = foldr step return xs z where 
    step x k = \acc -> k acc >>= f x  

foldrM''' f z xs = foldr step return xs z where 
    step x k = \acc -> 
        k acc >>= \r -> f x r 


foldrM f z xs = go xs where 
    go []       = return z
    go (x:xs)   = f x =<< unsafe xs  where
        unsafe ys = return (fix (\r -> case ys of 
            [] -> z
            _  -> z ))


{--
foldrM f z xs = go xs where 
    go [] = return z
    go (y:ys) = do
        tailRes <- unsafeInterleaveM (go ys) 
        f y tailRes
--}


u = undefined 


           
a = foldrM (\_ _ -> Nothing) 3 [] 
b = foldrM (\_ _ -> Nothing) u (u:u) 
c = foldrM (\_ z -> Left  z) 3 (u:u) 
d = foldrM (\i _ -> Left  i) u [u,2] 
e = foldrM (\_ z -> Right z) 3 [u,u] 
f = foldrM (\i _ -> Right i) u [1,u] 

