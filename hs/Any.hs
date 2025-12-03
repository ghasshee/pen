{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE DeriveFunctor #-} 
{-# LANGUAGE MonoLocalBinds #-} 


module Any where 

import Set 
import Semiring  
import Prelude hiding (any, (>>) ) 





data Any a  = Ex [a] 
            | In [a] 
            deriving (Functor) 


instance Applicative Any where 
    pure a = In [a] 
    In as <*> In bs = In (as <*> bs) 

instance Monad Any where
    In (a:as) >>= f = In (fa ++ fas) where 
        In fa  = f a 
        In fas = In as >>= f 


(~<) :: ListOperations [] a => a -> Any a -> Bool 
a ~< Ex as = a ∉ as  
a ~< In as = a ∈ as 





class Anyclass a where 
    none :: a 
    any  :: a 
    complement :: a -> a 

instance Anyclass (Any a) where 
    none = In [] 
    any  = Ex [] 
    complement (Ex l) = In l
    complement (In l) = Ex l 


instance (Ord a) => ListOperations Any a where 
    a >> In as          = In (a >> as) 
    a >> Ex as          = Ex as \\ In [a] 
    a ∈  In as          = a ∈ as 
    In as +++ In bs     = In (as +++ bs) 
    uniq (In as)        = In (uniq as) 
    uniq (Ex as)        = Ex (uniq as) 
    a ~=~ b             = undefined 



instance (Ord a, SetOperations [a] ) => SetOperations (Any a) where 
    subsets (In l)      = In <$> (subsets l) 
    In []   \\  _       = none 
    _       \\ Ex []    = none 
    Ex []   \\ a        = complement a 
    a       \\ In []    = a 
    In as   \\ In bs    = In (as \\ bs) 
    Ex as   \\ Ex bs    = Ex (bs \\ as) 
    Ex as   \\ In bs    = Ex (as ∩ bs) 
    In as   \\ Ex bs    = In (as ∩ bs) 
    In as   ∪  In bs    = In (as ∪ bs) 
    Ex as   ∪  Ex bs    = Ex (as ∩ bs) 
    In as   ∪  Ex bs    = Ex (bs \\ as) 
    Ex as   ∪  In bs    = Ex (as \\ bs)
    as      ∩  bs       = as ∪ (bs \\ as) 





jst :: a -> Any a 
jst = pure 

--{ Show }-- 

instance Show a => Show (Any a) where 
    show (In[])         = "ε"
    show (Ex[])         = "*"
    show (In(x:xs) )    = "{" ++ show x ++ loop xs  ++ "}" where 
        loop []     = ""
        loop (x:xs) = "," ++ show x ++ loop xs  
    show (Ex (x:xs) )   = "*\\{" ++ show x ++ loop xs ++ "}" where 
        loop []     = ""
        loop (x:xs) = "," ++ show x ++ loop xs  

--{ Read }-- 
instance Read a => Read (Any a) where 
    readsPrec _ "ε"             = [(In[], [])] 
    readsPrec _ "*"             = [(Ex[], [])] 
    readsPrec _ ('*':'\\':rest) = [(Ex l, [])] where 
        [(In l, _)]                 =  readsPrec 0 rest
    readsPrec _ ('{':xs)        = loop xs []    where 
        loop str as = case lex str of 
                [(a,'}':rest)]      -> [(In (reverse(read a:as)), [])] 
                [(a,',':rest)]      -> loop rest (read a:as) 
                [(a,' ':rest)]      -> loop (a++rest) as  

         
--{ Semigroup }--
instance Ord a => Semigroup (Any a) where 
    (<>)    = (∪) 

--{ Monoid }--
instance  Semigroup (Any a) => Monoid (Any a) where 
    mempty  = In [] 
    mappend = (<>) 

instance Eq a => Eq (Any a) where 
    Ex as == Ex bs                      = as ~=~ bs 
    In as == In bs                      = as ~=~ bs 
    _     == _                          = False  






instance Eq a => Ord (Any a) where 
    In []   <= _              = True
    _       <= Ex []          = True
    Ex []   <= a              = Ex [] == a 
    a       <= In []          = a == In [] 
    In(a:as)<= In l           = if a ∈ l then In as <= In l else False
    In(a:as)<= Ex l           = if a ∈ l then False else In as <= Ex l
    Ex(a:as)<= In l           = if a ∈ l 
                                         then Ex as <= In l 
                                         else Ex as <= In (a:l)
    Ex(a:as)<= Ex l           = if a ∈ l 
                                         then Ex as <= Ex (filter(/=a)l)
                                         else Ex as <= Ex l 


