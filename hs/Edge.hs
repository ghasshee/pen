module Edge where 

import Set
import Mapping 
import Node 
import Data.List (sort) 
import GHC.Exts
import Semiring 

type Edge node a = (node,a,node) 

instance {-# Overlapping #-} Semigroup a => Semigroup (Edge node a) where 
    (i,a,j) <> (_,b,_) = (i,a<>b,j) 
instance {-# Overlapping #-} Monoid a => Monoid (Edge node a) where 
instance Semiring a => Semiring (Edge Int a) where 
    (i,a,j) <.> (_,b,_) = (i, a<.>b, j) 
    (i,a,j) <+> (_,b,_) = (i, a<+>b, j)
    iszero (_,a,_)      = iszero a 


dom :: Edge node a -> node 
dom (q,_,_) = q

cod :: Edge node a -> node
cod (_,_,q) = q 

arrow :: Edge node a -> a 
arrow (_,a,_) = a 



{--
groupWith :: Ord b => (a -> b) -> [a] -> [[a]]
groupWith f xs = build (\c n -> groupByFB c n (\x y -> f x == f y) (sortWith f xs))

groupByFB :: ([a] -> lst -> lst) -> lst -> (a -> a -> Bool) -> [a] -> lst
groupByFB c n eq xs0 = groupByFBCore xs0
  where groupByFBCore [] = n
        groupByFBCore (x:xs) = c (x:ys) (groupByFBCore zs)
            where (ys, zs) = span (eq x) xs
--}

partition :: (Ord a, Ord b) => (a -> b) -> [a] -> [[a]] 
partition f as =  loop partition_len as [] where 
    partition_len = length <$> part (sort (f <$> as)) [[]]  
    loop [] [] ps = reverse ps  
    loop (len:lengths) as ps = loop lengths (drop len as) (take len as : ps)   
    


part [a]      (l:ls)                = reverse ((a:l):ls)                                               
part (a:b:xs) (l:ls) | a == b       = part (b:xs) ((a:l):ls) 
                     | otherwise    = part (b:xs) ([]:(a:l):ls)
    

sort_by :: (Ord a, Ord b) => (a -> b) -> [a] -> [a] 
sort_by f as = snd <$> sort (zip (f <$> as) as)  

dom_partition :: (Ord a, Ord node) =>  [Edge node a] -> [[Edge node a]]
dom_partition = groupWith dom 


leftEdge (p,a,q)  = (Left p, a, Left q)
rightEdge (p,a,q) = (Right p, a, Right q) 
leftEdges es = leftEdge <$> es 
rightEdges es = rightEdge <$> es 

renodeEdge :: (Eq node, Num s, Enum s) => Map node (Node s) -> Edge node a -> Edge (Node s) a 
renodeEdge table (p,a,q)            = (renode table p, a, renode table q) 

renodeEdges :: (Eq node, Num s, Enum s) => Map node (Node s) -> [Edge node a] -> [Edge (Node s) a]
renodeEdges table es                = renodeEdge table <$> es 

intersect_edges :: Eq a => [(Edge node a,Edge node a)] -> [Edge (node,node) a]
intersect_edges (((q,a,p),(s,b,r)):xs)  | a == b    = ((q,s),a,(p,r)) : intersect_edges xs  
                                        | otherwise =                   intersect_edges xs 



