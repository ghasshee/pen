

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


x = XS (XS (XS (XZ [[[], [1], [2,1],[3,2,1]], [[]], [[], [1]]]))) 





