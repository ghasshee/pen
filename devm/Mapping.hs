
module Mapping where 


import Set 

type Map a b = [(a,b)] 



table :: [a] -> [b] -> Map a b 
table = zip 


mapping :: Eq a => Map a b -> a -> b 
mapping  [] a                        = error "partial table" 
mapping  ((x,y):xs) a    | a == x    = y
                         | otherwise = mapping xs a 


mapping_all :: Eq a => Map a b -> [a] -> [b] 
mapping_all tbl as = map (mapping tbl) as
