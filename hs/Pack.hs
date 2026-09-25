module Pack where 

import Tree 
import Term
import Type 

type Arity = Int 

type Tag    = Int 
data Pack   = Pack Int Arity 
     

-- Tag 
-- 0 -> TyU256
-- 1 -> TyADDR    
-- RESERVED tag
--
-- 2 -> Node
-- 3 -> Leaf 
--
-- 
            
    deriving (Eq, Read, Show) 

ty2tag :: Ty -> Tag 
ty2tag TyU256 = 0 
ty2tag TyADDR = 1 

pack :: Term -> Pack 
pack (RED (TmCON n id) trs) = undefined  



    {--
Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf) 

--> Pack Array 
--  [ Pack 2 3 ; Pack 0 _; Pack 2 3; Pack 0 _;Pack 3 0; Pack 3 0; Pack 2 3; Pack 0 _; Pack 3 0; Pack 3 0 ] 
--
--> Value Array
--  [ 1 ; 2 ; 3 ]

--}

type Pointer = Int 

--data Pack' = Pack Int Arity Pointer 

packTmCON :: Term -> Pack 
packTmCON (RED (TmCON n id) trs) = Pack 0 0








    {-- Mutual Recursion
data Forest a = List (Tree a)

data Tree a = Leaf a | Node (Forest a) 

mu X . lam a. a + (mu Y. 1 + X * Y) 
--}



{--

PACK(2,0)
PUSH1 3
PACK(1,2) 
PUSH1 2 
PACK(1,2)
PUSH1 1 
PACK(1,2) 

--}



-- eval : Constructed Data Structure 
-- finally burn into : Storage 
--
--
-- e.g.  cons 1 (cons 2 ( cons 3 nil)  
-- is burnt at Storage as an array [1,2,3] 
--
-- e.g. tree is similar 
--
--
-- we cannot pass datatypes as argument of methods. 
-- we can pass datatypes as argument of functions. 
--
-- if we pass datatypes to a function, 
-- then they are pointer addresses
-- 
-- pointer addresses are deconstructed in the TmCase deconstructor in the function.
--
--
--  

