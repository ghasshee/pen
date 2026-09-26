module Data2Functor where 

import AST
import Data
import Type
import Utils 
import Functor




-- Data => Functor 
-- see the defintion of Functor 
-- * Functor.hs 

d2F :: DInd -> F Ty String
d2F (DInd id ps [c]   )         = c2F id ps c 
d2F (DInd id ps (c:cs))         = FSum (c2F id ps c) (d2F (DInd id ps cs))

c2F :: ID -> [ID] -> DConstr -> F Ty String 
c2F id ps (DConstr cid []      ) = FOne 
c2F id ps (DConstr cid [ty]    ) = ty2F id ty 
c2F id ps (DConstr cid (ty:tys)) = FProd (ty2F id ty) (c2F id ps (DConstr cid tys))

ty2F :: ID -> Ty -> F Ty String 
ty2F id (TyID s)       | s == id   = FVar s 
ty2F id ty                         = FConst ty 

dt2F :: TOP -> F Ty String
dt2F (DT id _ ps cs) = d2F (DInd id ps cs) 

f2ty :: F Ty String -> Ty 
f2ty (FVar x) = TyID x 


l' = d2F l
n' = d2F n 


--data List' a = Nil' | Cons' a (List' Int) (List' a)  



