module Functor where 

import Fix 
import Type 
import Semiring 


-- Functor Type f 
data FTy x  = FZero   
            | FOne 
            | FVar x   -- Recursive Parameter 
            | FProd (FTy x) (FTy x) 
            | FSum  (FTy x) (FTy x) 
            | FConst Ty  
            deriving (Eq) 


instance Functor FTy  where 
    fmap _ FZero = FZero 
    fmap _ FOne  = FOne 
    fmap f (FVar x) = FVar (f x) 
    fmap f (FProd a b) = FProd (fmap f a) (fmap f b) 
    fmap f (FSum a b)  = FSum (fmap f a) (fmap f b) 


instance Show (FTy String) where 
    show FZero          =   "0" 
    show FOne           =   "1" 
    show (FVar x)       = x 
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

instance Semigroup (FTy String)  where 
    a <> b              = FProd a b 
instance Monoid (FTy String)  where 
    mempty              = FOne 
instance Semiring (FTy String)  where 
    zero                = FZero 
    a <+> b             = FSum a b 
    iszero a            = a == FZero 

instance Num (FTy String) where 
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



instance Show (Fix FTy) where 
    show = show' . showFix show 

