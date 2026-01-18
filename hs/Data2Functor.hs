module Data2Functor where 

import AST
import Data
import Type
import Utils 
import Functor

import Data.Char (isUpper)



d2F :: DInd -> F Ty String
d2F (DInd id ps [c]   ) = c2F id ps c 
d2F (DInd id ps (c:cs)) = FSum (c2F id ps c) (d2F (DInd id ps cs))

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



data2type :: DInd -> Ty 
data2type (DInd id ids cs) = TyREC id (loop ids cs) where 
    loop []     cs  = constrs2type cs  
    loop (i:is) cs  = TyABS i (loop is cs) 

constrs2type []     = TyERR
constrs2type [c]    = constr2type c
constrs2type (c:cs) = TySUM (constr2type c) (constrs2type cs) 

constr2type (DConstr id tys) = loop tys where 
    loop []         = TyUNIT
    loop [ty]       = ty
    loop (ty:tys)   = TyPAIR ty (loop tys) 


typedDT :: TOP -> TOP
typedDT (DT id _ ids cs) = DT id (ty:contys) ids cs' where 
    ty      = data2type (DInd id ids cs) 
    contys  = data2contype (DInd id ids cs) 
    cs'     = typedDConstrs contys cs

typedDConstrs :: [Ty] -> [DConstr] -> [DConstr]
typedDConstrs (cty:ctys) (DConstr id _ : ds ) = DConstr id [cty] : typedDConstrs ctys ds 



data2contype :: DInd -> [Ty] 
data2contype (DInd id ids cs) = constr2contype ret <$> cs where 
    ret                 = loop ids (TyD id)   
    loop []       id    = id 
    loop (ty:tys) id | isUpper (hd ty)  = loop tys (TyAPP id (TyD ty)) 
                     | otherwise        = loop tys (TyAPP id (TyID ty))

constr2contype :: Ty -> DConstr -> Ty 
constr2contype ret (DConstr id tys) = TyCON id (loop tys ret) where 
    loop [] ret = ret
    loop (ty:tys) ret = TyARR ty (loop tys ret) 



