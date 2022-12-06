module Kont where 

import Data.Dynamic

newtype Output a = O { rO :: (a, String) } 

instance Functor Output where 
    fmap f (O(a,s)) = O(f a,s)  

instance Applicative Output where 
    pure = return 
    (<*>) = undefined 
instance Monad Output where 
    return a =  O (a, "")
    o >>= f  =  let (a ,s ) = rO o in 
                let (a',s') = rO (f a) in 
                O (a', s ++ s') 


out :: Char -> Output () 
out c = O ((),[c])

collect :: Output () -> String 
collect o = snd (rO o) 



newtype Output' a = O' { rO' :: String -> (a, String) } 

instance Functor Output' where
    fmap f (O' t) = O' (\s -> let (a,s') = t s in (f a, s'))
instance Applicative Output' where 
    pure = return 
    (<*>) = undefined 
instance Monad Output' where 
    return a    = O' (\s -> (a,s))
    o >>= f     = O' (\s -> let (a,s') = rO' o s in rO' (f a) s')


out'    :: Char -> Output' () 
out' c  = O' (\s -> ((), c : s))

collect' :: Output' () -> String 
collect' o = let ((),s) = rO' o [] in reverse s 

flush :: Output' ()     -- erase all output so far 
flush = O' (\s -> ((), ""))

peek :: Output' Char    -- return last char output
peek = O' (\s -> (head s, s))



-- Connecting Output and Output' 
-- Output : values
-- Output' : behaviors

reflectO :: Output a -> Output' a
reflectO (O(a,s)) = O' (\s' -> (a, reverse s ++ s')) 

reifyO   :: Output' a -> Output a
reifyO (O' t) = let (a,s) = t [] in O (a, reverse s) 








newtype Cont r a = C { rC :: (a -> r) -> r }


instance Functor (Cont r) where 
    fmap        = undefined 
instance Applicative (Cont r) where 
    pure        = return 
    (<*>)       = undefined 
instance Monad (Cont r) where 
    return a    = C (\k -> k a) 
    C r >>= f   = C (\k -> r (\a -> rC (f a) k)) 


class Monad m => PCRMonad m where 
    pcreflect   :: m a -> Cont (m d) a
    pcreify     :: (forall d. Cont (m d) a) -> m a

instance PCRMonad (Cont r) where 
    pcreflect m     = C (\k -> k =<< m)
    pcreify (C r)   = r return  



type Output'' a = forall d. (a -> Output d) -> Output d 
out'' :: Char -> Cont ((),String) String 
out'' c = C (\k -> ((), [c]) >>= k) 



type Ans = Dynamic 

fmDyn d = fromDyn d (error "Dynamic") 

newtype ContState s x = CS {rCS :: (x -> s -> Ans) -> s -> Ans }

