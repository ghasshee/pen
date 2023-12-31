-- {-# LANGUAGE RankNTypes #-} 
--
--
-- references : 
-- - https://gist.github.com/lotz84/7fd7e279bd7196c6baab
-- - https://dl.acm.org/doi/pdf/10.1145/3236779



module Lens where 


import Comonad 

data Store k v = Store (k -> v) k

instance Functor (Store k) where 
    fmap f (Store m k) = Store (f . m) k

instance Comonad (Store k) where 
    extract   (Store m k) = m k
    duplicate (Store m k) = Store (Store m) k  




-- k is global datatype 
-- v is local datatype
type Lens' k v = k -> Store v k  


lens' :: (k -> v) -> (k -> v -> k) -> Lens' k v 
lens' get set k = Store (\v -> set k v) (get k) 

get' :: Lens' k v -> k -> v
get' l v = let Store _ k = l v in k

set' :: Lens' k v -> k -> v -> k 
set' l v = let Store m _ = l v in m 


-- 
-- e.g.

_fst' :: Lens' (a,b) a 
_fst' = lens' fst setFst 
    where setFst (_,y) x = (x,y) 

_snd' :: Lens' (a,b) b 
_snd' = lens' snd setSnd

    where setSnd (x,_) y = (x,y) 



data Lens a b s t = Lens { view :: s -> a , update :: (s,b) -> t } 

class Profunctor p where 
    dimap :: (c -> a) -> (b -> d) -> p a b -> p c d 

-- Cartesian Profunctor is a profunctor coherent with products
class Profunctor p => Cartesian p where 
    second :: p a b -> p (c,a) (c,b) 

type LensP a b s t = forall p. Cartesian p => p a b -> p s t



-- e.g. 
sndLens :: Lens a b (c,a)(c,b)
sndLens = Lens vw up where 
    vw(c,a) = a 
    up((c,a),a') = (c,a')


-- Dually, Prism provides access onto a component within a composite sum data structure

data Prism a b s t = Prism { match :: s -> Either t a, build :: b -> t } 

-- e.g. 
the :: Prism a b (Maybe a)(Maybe b)
the = Prism mt bd where 
    mt (Just a) = Right a 
    mt Nothing  = Left Nothing
    bd b        = Just b


-- Lenses and Prisms turn out to be divergent generalizations of Adapters

data Adapter a b s t = Adapter { from :: s -> a, to :: b -> t } 

flatten :: Adapter (a,b,c) (a',b',c') ((a,b),c) ((a',b'),c')
flatten = Adapter from to where 
    from ((a,b),c) = (a,b,c) 
    to  (a,b,c) = ((a,b),c) 






nth :: (Num n, Eq n) => n -> Lens a a [a] [a] 
nth n = Lens (vw n) (up n) where 
    vw n []         = error "nth element does not exist" 
    vw 0 (x:xs)     = x 
    vw n (x:xs)     = vw (n-1) xs 
    up n ([],a')    = error "nth element does not exist"
    up 0 (x:xs,a')  = a':xs 
    up n (x:xs,a')  = x : up (n-1) (xs,a') 


eg = update (nth 3) ([1..10], 99) 


