{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll  #-}

module Yoneda where 


data Yo f a = Yo {unYo :: forall r. (a->r) -> f r } 


fromYo :: Yo f a -> f a 
fromYo y = unYo y id 

toYo   :: Functor f => f a -> Yo f a 
toYo   x  = Yo (\h -> fmap h x) 



data CoYo f r = forall a . Coyo { unCoYo :: (f a, a -> r) } 
