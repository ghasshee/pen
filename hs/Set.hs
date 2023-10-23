module Set where 

import Data.List (sort) 


mkset l = (sort . uniq) l 


-- || generate SET from LIST || -- 

uniq :: Eq a => [a] -> [a] 
uniq [] = [] 
uniq (a:as) = if a `elem` as then uniq as else a : uniq as 


-- || generate all SUBSETS || -- 

subsets [] = [[]] 
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

-- || Logical Operations || --

intersect [] bs = [] 
intersect (a:as) bs = if a `elem` bs then a : intersect as bs else intersect as bs 

-- ||  +  &  -   operations on sets || -- 

removeset :: Eq a =>  [a] -> a -> [a] 
removeset [] x              = [] 
removeset (a:as) x | x==a   = as 
removeset (a:as) x          = a : removeset as x

setminus  :: Eq a => [a] -> [a] -> [a] 
setminus l1 []      = l1 
setminus l1 (x:xs)  = setminus (removeset l1 x) xs  

addset    :: Eq a =>  a  -> [a] -> [a] 
addset x l              =   if x `elem` l then l else x : l 

setplus   :: Eq a => [a] -> [a] -> [a] 
setplus  l1 []      = l1
setplus  l1 (x:xs)  = setplus  (addset    x l1) xs 

infixr 1 >>
(>>)  :: Eq a => a -> [a] -> [a] 
(>>) = addset


--addset2 :: [a] -> a -> a -> [a] 
--addset2 l x1 x2         =   addset (addset l x1) x2 

addsetifnot :: Eq a => [a] -> [a] -> a -> [a] 
addsetifnot m l x       =   if x `elem` m then l else addset x l

addsetifnot2 :: Eq a => [a] -> [a] -> a -> a  -> [a] 
addsetifnot2 m l x1 x2  =   addsetifnot m (addsetifnot m l x1) x2
