module Data2Type where 

import AST
import Data
import Type 
import Utils
import Data.Char (isUpper) 


-- Data => Ty
-- ( the prototype .. Data2Functor.hs ) 




data2type :: DInd -> Ty 
data2type (DInd id ids cs) = TyREC id (loop ids cs) where 
    loop []     cs      = constrs2type cs  
    loop (i:is) cs      = TyABS i (loop is cs) 


constrs2type    :: [DConstr] -> Ty 
constrs2type []         = TyERR
constrs2type [c]        = constr2type c
constrs2type (c:cs)     = TySUM (constr2type c) (constrs2type cs) 


constr2type     ::  DConstr  -> Ty 
constr2type (DConstr id tys) = loop tys where 
    loop []             = TyUNIT
    loop [ty]           = ty
    loop (ty:tys)       = TyPAIR ty (loop tys) 


annotateDT :: TOP -> TOP
annotateDT (DT id _ ids cs) = DT id (ty:contys) ids cs' where 
    ty      = data2type     (DInd id ids cs) 
    contys  = data2contypes (DInd id ids cs) 
    cs'     = annotateDConstrs contys cs


annotateDConstrs :: [Ty] -> [DConstr] -> [DConstr]
annotateDConstrs []         []                   = [] 
annotateDConstrs (cty:cs) (DConstr i _:ds) = DConstr i [cty] : annotateDConstrs cs ds 




data2contypes :: DInd -> [Ty] 
data2contypes (DInd id ids cs) = constr2contype ret <$> cs where 
    ret                 = loop ids (TyD id)   
    loop []       id    = id 
    loop (ty:tys) id | isUpper (hd ty)  = loop tys (TyAPP id (TyD ty)) 
                     | otherwise        = loop tys (TyAPP id (TyID ty))

constr2contype :: Ty -> DConstr -> Ty 
constr2contype ret (DConstr id tys) = TyCON id (loop tys ret) where 
    loop []         ret = ret
    loop (ty:tys)   ret = TyARR ty (loop tys ret) 



