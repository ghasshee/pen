module OR where 

import Semiring

{--
data BinListErr = Zr
                | Sq [a] 
                | Br (Bin* a) (Bin* a) deriving (Eq,Read) 
--}

data OR a       = ZR
                | SQ [a]  
                | OR (OR a) (OR a)  deriving (Eq,Read) 


instance Show a => Show (OR a) where 
    show (ZR        )   = "0" 
    show (SQ l      )   = show l
    show (OR a b    )   = show a ++ "  |  " ++ show b 
    

instance Semigroup (OR a) where 
    SQ l <> SQ m                = SQ (l <> m)
    OR t s <> o                 = OR (t <> o) (s <> o) 
    o <> OR t s                 = OR (o <> t) (o <> t) 

instance Monoid (OR a) where 
    mempty                      = SQ []
    mappend ZR  a               = a 
    mappend a  ZR               = a 
    mappend (SQ l) (SQ m)       = SQ (mappend l m)  
    mappend (OR t s) o          = OR (mappend t o) (mappend s o) 
    mappend o (OR t s)          = OR (mappend o t) (mappend o s) 

instance Semiring (OR a) where 
    zero            = ZR 
    one             = mempty 
    ZR    <+> a     = a 
    a     <+> ZR    = a 
    SQ [] <+> a     = a 
    a     <+> SQ [] = a 
    a     <+> b     = OR a b 
    ZR    <.> a     = ZR 
    a     <.> ZR    = ZR 
    SQ [] <.> a     = a 
    a     <.> SQ [] = a 
    a     <.> b     = mappend a b 

instance Functor OR where 
    fmap f ZR       = ZR
    fmap f (SQ l)   = SQ (fmap f l) 
    fmap f (OR a b) = OR (fmap f a) (fmap f b) 





