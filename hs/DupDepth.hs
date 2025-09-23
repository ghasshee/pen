module DupDepth where 



import Term
import Tree
import Type 






type DupDepth = [(Int,Int)]

d_push     :: Int -> DupDepth -> DupDepth 
d_push 0 ds = ds 
d_push d ds = (d,0) : ds 

d_dup []                = []  
d_dup ((d,dup):ds)      = (d,dup+1) : ds   

d_minus ((0,_  ):ds)    = ds 
d_minus ((d,dup):ds)    = ((d-1,dup):ds) 

dup_sum []            = 0   
dup_sum ((d,dup):ds)  = dup + dup_sum ds  

