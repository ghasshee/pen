module DupDepth where 



import Term
import Tree
import Type 



-- 
-- Dup Depth 
--  Duplication Depth in Term Tree
--
--  [(l,r)] where 
--
--   l := # of Branched(args) in the current branch(term)
--   r := # how many times (DUP i) is generated through the tree to PG compilation in the level 
--
--   In the tree walking, 
--   a level is created if we go down into a new subtree,
--   and eliminated if the subtree walk is finished.


type DupDepth = [(Int,Int)]

d_push     :: Int -> DupDepth -> DupDepth 
d_push 0 ds = ds 
d_push d ds = (d,0) : ds 

d_dup []                = []  
d_dup ((d,dup):ds)      = (d,dup+1) : ds   

d_minus []              = [] 
d_minus ((0,_  ):ds)    = ds 
d_minus ((d,dup):ds)    = ((d-1,dup):ds) 

dup_sum []            = 0   
dup_sum ((d,dup):ds)  = dup + dup_sum ds  



