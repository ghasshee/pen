module Set where 




removeset :: Eq a =>  [a] -> a -> [a] 
removeset [] x              = [] 
removeset (a:as) x | x==a   = as 
removeset (a:as) x          = a : removeset as x

setminus :: Eq a => [a] -> [a] -> [a] 
setminus l1 []      = l1 
setminus l1 (x:xs)  = setminus (removeset l1 x) xs  

addset :: Eq a => a -> [a] -> [a] 
addset x l              =   if x `elem` l then l else x : l 

infixr 1 >>
(>>)  :: Eq a => a -> [a] -> [a] 
(>>) = addset


--addset2 :: [a] -> a -> a -> [a] 
--addset2 l x1 x2         =   addset (addset l x1) x2 

addsetifnot :: Eq a => [a] -> [a] -> a -> [a] 
addsetifnot m l x       =   if x `elem` m then l else addset x l

addsetifnot2 :: Eq a => [a] -> [a] -> a -> a  -> [a] 
addsetifnot2 m l x1 x2  =   addsetifnot m (addsetifnot m l x1) x2
