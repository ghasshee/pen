module Functor where 

import Fix 
import Semiring 

data Mu ty x = Mu x (F ty x)  

-- Functor Type f 
data F ty x = FZero   
            | FOne 
            | FVar x   -- Recursive Parameter 
            | FProd (F ty x) (F ty x) 
            | FSum  (F ty x) (F ty x) 
            | FConst ty  
            deriving (Eq) 


instance Functor (F ty) where 
    fmap _  FZero       = FZero 
    fmap _  FOne        = FOne 
    fmap f (FVar x)     = FVar  (f x) 
    fmap f (FProd a b)  = FProd (fmap f a) (fmap f b) 
    fmap f (FSum  a b)  = FSum  (fmap f a) (fmap f b) 


instance Show ty => Show (F ty String) where 
    show (FZero )       =   "0" 
    show (FOne  )       =   "1" 
    show (FVar x)       =    x 
    show (FProd(FSum a b)(FSum c d)) 
                        =   "(" ++ show(FSum a b) ++ ")" ++ " × " ++
                            "(" ++ show(FSum c d) ++ ")" 
    show (FProd(FSum a b)c)   
                        =   "(" ++ show(FSum a b) ++ ")" ++ " × " ++ show c
    show (FProd a(FSum b c)) 
                        =   show a ++ " × " ++ "(" ++ show (FSum b c) ++ ")"
    show (FProd a b)    =   show a ++ " × " ++ show b 
    show (FSum  a b)    =   show a ++ " + " ++ show b 
    show (FConst ty)    =   show ty 


instance Eq ty => Semigroup (F ty String)  where 
    a <> b              = FProd a b 
instance Eq ty => Monoid (F ty String)  where 
    mempty              = FOne 
instance Eq ty => Semiring (F ty String)  where 
    zero                = FZero 
    a <+> b             = FSum a b 
    iszero a            = a == FZero 


instance Eq ty => Num (F ty String) where 
    (+)     = (<+>) 
    (*)     = (<.>) 
    (-)     = undefined 
    abs     = undefined 
    signum  = undefined 
    fromInteger n = loop n where 
        loop 0 = FZero  
        loop 1 = FOne 
        loop n  | n`mod`2 == 0  = FProd (FSum FOne FOne) (loop (n`div`2))  
                | otherwise     = FSum FOne (loop (n - 1)) 


instance Show ty => Show (Fix (F ty)) where 
    show = show' . showFix show 

