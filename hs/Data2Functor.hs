module Data2Functor where 

import AST
import Data
import Type
import Functor

d2F :: DInd -> F String
d2F (DInd id ps [c]   ) = c2F id ps c 
d2F (DInd id ps (c:cs)) = FSum (c2F id ps c) (d2F (DInd id ps cs))

c2F :: ID -> [ID] -> DConstr -> F String 
c2F id ps (DConstr cid []      ) = FOne 
c2F id ps (DConstr cid [ty]    ) = ty2F ty 
c2F id ps (DConstr cid (ty:tys)) = FProd (ty2F ty) (c2F id ps (DConstr cid tys))

ty2F :: Ty -> F String 
ty2F (TyDATA s tys)     = FVar s 
ty2F ty                 = FConst ty 

dt2F :: TOP -> F String
dt2F (DT id ps cs) = d2F (DInd id ps cs) 


l' = d2F l
n' = d2F n 

