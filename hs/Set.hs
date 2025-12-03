{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE MonoLocalBinds #-} 
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE GADTs #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 

module Set where 

import Data.List (sort)  
import Prelude hiding ((>>)) 


-- Set Utilities 
import Data.Set (Set)
import qualified Data.Set as S



instance Eq a => SetOperations (Set a) where 
    φ           = S.empty 
    

instance (Eq a , Ord a) => ListOperations Set a where 
    ( ∈ )       = S.member 
    ( >> )      = S.insert 
    toList      = S.toList
    fromList    = S.fromList
    



class Eq s => SetOperations s where    
    φ           :: s 
    empty       :: s 
    subsets     :: s -> [s]  
    power       :: s -> [s]  
    intersect   :: s -> s -> s 
    union       :: s -> s -> s 
    diff        :: s -> s -> s 
    (∩)         :: s -> s -> s  
    (∪)         :: s -> s -> s  
    (⊂)         :: s -> s -> Bool 
    (⊃)         :: s -> s -> Bool 
    (\\)        :: s -> s -> s 
    φ           = empty 
    power       = subsets 
    intersect   = (∩)
    union       = (∪)
    a ⊂ b       = (a ∩ b == a) 
    b ⊃ a       = a ⊂ b 
    diff        = (\\)
    infixl 8 ∩  
    infixl 7 ∪
    infixl 6 \\ 

class ListOperations s a where 
    toList          :: s a -> [a] 
    fromList        :: [a] -> s a  
    (∈)             :: a -> s a -> Bool 
    (∉)             :: a -> s a -> Bool 
    (~=~)           :: s a -> s a -> Bool 
    (>>)            :: a -> s a -> s a 
    add             :: a -> s a -> s a
    add             = (>>) 
    uniq            :: s a -> s a  
    disjointunion   :: s a -> s a -> s (Either a a) 
    (+++)           :: s a -> s a -> s (Either a a)
    disjointunion   = (+++)
    a ∉ l           = not (a ∈ l)    
    infixr 3 ~=~ 
    infixl 5 ∈    
    infixr 7 >> 
    
instance Eq a => SetOperations [a] where 
    subsets []                  = [[]]
    subsets (x:xs)              = subsets xs ++ ( (x:) <$> subsets xs)  
    []      ∩ _                 = [] 
    _       ∩ []                = []  
    (a:as)  ∩ bs | a ∈ bs       = a : bs ∩ as 
                  | otherwise   = bs ∩ as 
    as      ∪ bs                = uniq (as ++ bs) 
    as      \\  []              = as 
    as      \\ (b:bs)           = filter (/=b) as \\ bs 
    

instance Eq a => ListOperations [] a where 
    a ∈ b                       = a `elem` b  
    []   ~=~ []                 = True
    []   ~=~ _                  = False 
    a:as ~=~ l  | a ∈ l         = as ~=~ (filter (/=a) l) 
                | otherwise     = False  
    a >> l      | a ∈ l         = l 
                | otherwise     = a:l 
    l +++ r                     = (Left <$> l) ++ (Right <$> r )
    uniq []                     = [] 
    uniq (a:as) | a ∈ as        = uniq as 
                | otherwise     = a : uniq as 





    {--
-- { Set type }-- 

newtype Set a = Set [a] 

unset       (Set l)     = l 
set            l        = (Set . sort . uniq) l 

instance Show a => Show (Set a) where 
    show (Set l) = '{' : init ( drop 1 (show l) ) ++ "}" 
          
instance Ord a => SetOperations (Set a) where 
    subsets         = (Set <$>) . subsets . unset 
    Set l ∩ Set k   =  Set ( l ∩ k ) 
    Set l ∪ Set k   =  Set ( l ∪ k ) 
    Set l \\ Set k  =  Set ( l \\ k ) 
    uniq (Set l)    =  Set (uniq l) 

instance Ord a => ListOperations Set a where 
    a ∈ Set l       =  a ∈ l 
    Set l ~=~ Set k =  l ~=~ k 
    a >> Set l      =  Set (a >> l) 
    Set l +++ Set r = (Set $ Left <$>  l) ∪ (Set $ Right <$> r) 

instance Eq a => Eq (Set a) where 
    Set l == Set k  = l ~=~ k  

instance Eq a => Ord (Set a) where 
    Set []      <= Set _              = True
    Set (a:as)  <= Set l  | a ∈ l     = True 
                          | otherwise = False 

--}



addifnot :: Eq a => [a] -> [a] -> a -> [a] 
addifnot m l x       =   if x ∈ m then l else x >> l 

addifnot2 :: Eq a => [a] -> [a] -> a -> a  -> [a] 
addifnot2 m l x1 x2  =   addifnot m (addifnot m l x1) x2
