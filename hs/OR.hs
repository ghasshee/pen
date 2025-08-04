module OR where 

import Semiring
import Set 

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
    

instance Eq a => Ord (OR a) where 
    SQ l > SQ m                 = l ⊃ m 
    (<=)                        = (.)(.)(.) not (>)



instance Semigroup (OR a) where 
    ZR   <> _                   = ZR 
    _    <> ZR                  = ZR
    SQ[] <> a                   = a 
    a    <> SQ[]                = a 
    SQ l <> SQ m                = SQ (l <> m)
    OR t s <> o                 = OR (t <> o) (s <> o) 
    o <> OR t s                 = OR (o <> t) (o <> s) 

instance Monoid (OR a) where 
    mempty                      = SQ []
    mappend                     = (<>) 

instance Eq a => Semiring (OR a) where 
    zero            = ZR 
    one             = mempty 
    ZR    <+> a     = a 
    a     <+> ZR    = a 
    SQ [] <+> a     = a             -- WARNING : 1 + a = a 
    a     <+> SQ [] = a             -- WARNING : 1 + a = a 
    a     <+> b     = OR a b 
    a     <.> b     = a <> b 
    iszero a        = a == ZR 

instance Functor OR where 
    fmap f ZR       = ZR
    fmap f (SQ l)   = SQ (fmap f l) 
    fmap f (OR a b) = OR (fmap f a) (fmap f b) 





unwrapOR :: OR a -> Maybe a 
unwrapOR (ZR)       = Nothing
unwrapOR (SQ [a])   = Just a 
unwrapOR _          = error "cannot unwrap OR" 


