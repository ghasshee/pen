{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE TypeFamilies #-} 

module Yoneda where 


data Yo f a = Yo {unYo :: forall r. (a->r) -> f r } 


fromYo :: Yo f a -> f a 
fromYo y = unYo y id 

toYo   :: Functor f => f a -> Yo f a 
toYo   x  = Yo (\h -> fmap h x) 



data CoYo f r = forall a . CoYo { unCoYo :: (f a, a -> r) } 


fromCoYo :: Functor f => CoYo f b -> f b 
fromCoYo (CoYo (x,h)) = fmap h x 

toCoYo  :: f b -> CoYo f b 
toCoYo y = CoYo (y,id) 






class Functor f => Naperian f where 
    type Log f 
    lookup :: f a -> (Log f -> a) -- each other's 
    tabulate :: (Log f -> a) -> f a -- ... inverses
    positions :: f (Log f) 

    tabulate h = fmap h positions 
    positions = tabulate id 
