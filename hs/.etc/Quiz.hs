

data X f a  = XZ a                  -- XZ :: a -> X f a 
            | XS (X f (f a))        -- XS :: X f (f a) -> X f a 

data Y f a  = YZ a                  -- YZ :: a -> Y f a 
            | YS (f (Y f a))        -- YS :: f (Y f a) -> Y f a 



instance Show a => Show (X [] a) where 
    show (XZ a)     = "XZ " ++ show a 
    show (XS xffa)  = "XS(" ++ show xffa ++ ")"

instance Show a => Show (Y [] a) where 
    show (YZ a)     = "YZ " ++ show a 
    show (YS fyfa)  = "YS"  ++ show fyfa  



embed :: Functor f => X f a -> Y f a 
embed (XZ a)    = YZ a 
embed (XS xffa) = loop $ embed xffa where 
    loop (YZ fa)        = YS $ YZ   <$> fa    
    loop (YS f_Yffa)    = YS $ loop <$> f_Yffa 



embed' :: Functor f => X f a -> Y f a 
embed' x = go x YZ where 
    go :: Functor f => X f b -> (b -> Y f a) -> Y f a 
    go (XZ a)       k  = k a 
    go (XS xffa)    k  = go xffa (YS  . (k <$>) ) 

x = XS (XS (XS (XZ [[[], [1..10000], [1..1000],[1..100]], [[]], [[], [1]]]))) 




--foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b

foldrM f z xs = go xs where 
    go []   = return z 
    go (x:xs) = do 
        t <- go xs 
        f x t


--foldrM cons nil l | null l = pure nil
--foldrM cons nil l = foldr (\a mb -> mb >>= cons a) (cons u nil) l 
--foldrM cons nil l   = foldl (\mb a -> mb >>= cons a) (pure nil) 
--                    | otherwise     = undefined 
{--
foldrM f z xs = (foldr step return xs
    where 
        step a k = do 
            z' <- f a k 
   --}      


c = foldrM (\a b -> Just (a+b)) 0 [1..10] 

-- foldl (b -> a -> b) -> b -> t a -> b 


u = undefined 

a = foldrM (\_ _ -> Nothing) 3 [] == Just 3
b = foldrM (\_ _ -> Nothing) undefined (undefined:undefined)





