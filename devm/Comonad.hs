module Comonad where 

-- import Aop.Tree



class Functor w => Comonad w where
    extract     :: w a -> a 
    extend      :: (w b -> a) -> w b -> w a
    duplicate   :: w a -> w (w a) 

    duplicate   = extend id 
    extend f    = fmap f . duplicate 


--{  List Zipper  }-- 
data Z a                = Z [a] a [a] 

left, right             :: Z a -> Z a 
left  (Z (l:ls) c rs)   = Z ls l (c:rs) 
right (Z ls c (r:rs))   = Z (c:ls) r rs 


iterate1 :: ( a -> a) -> a -> [a] 
iterate1 f = tail . iterate f 


instance Functor Z where 
    fmap f (Z ls c rs) = Z (fmap f ls) (f c) (fmap f rs) 

instance Comonad Z where 
    extract (Z _ a _ )  = a 
    duplicate z         = Z (iterate1 left z) z (iterate1 right z) 
    extend f z          = Z (fmap f $ iterate1 left z) (f z) (fmap f $ iterate1 right z) 

toZ :: a -> [a] -> Z a 
toZ a xs =  Z (repeat a) a (xs ++ repeat a) 


--{  2D List Zipper  }-- 
newtype Z2 a = Z2 (Z(Z a))

instance Functor Z2 where 
    fmap f (Z2 zz)      = Z2 (fmap (fmap f) zz)

instance Comonad Z2 where 
    extract (Z2 zz)     = extract (extract zz) 
    duplicate (Z2 zz)   = fmap Z2 . Z2 . roll $ roll zz where 
        roll zz = Z (iterate1 (fmap left) zz) zz (iterate1 (fmap right) zz) 


toZ2 :: a -> [[a]] -> Z2 a 
toZ2 a xss = Z2 $ toZ (toZ a []) (map (toZ a) xss) 




    
