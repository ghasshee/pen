module Datatype where 

import Hex
import Fix 
import Functor
import Type 
import LTree
import Utils 
import Semiring 





-- Inductive Data Type 
data DInd       = DInd ID [ID] [DConstr] 

data DConstr    = DConstr ID [Ty] 
                deriving (Show, Eq, Read) 



dInd2FTy (DInd id args [c])    = dConstr2FTy id args c 
dInd2FTy (DInd id args (c:cs)) =  
    FSum (dConstr2FTy id args c) (dInd2FTy (DInd id args cs))

dConstr2FTy id args (DConstr cid []      ) = FOne 
dConstr2FTy id args (DConstr cid [ty]    ) = ty2FTy id args ty 
dConstr2FTy id args (DConstr cid (ty:tys)) = 
    FProd (ty2FTy id args ty) (dConstr2FTy id args (DConstr cid tys))


ty2FTy id args (TyDATA s tys) | id == s = FVar "List" 
ty2FTy id args ty                       = FConst ty 



-- e.g. 
-- defining List 

l = DInd "List" ["a"] 
        [DConstr "Nil"  []
        ,DConstr "Cons" [TyID "a", TyDATA "List" [TyID "a"]]
        ] 






    {--
# solc 

# mapping 
# variable 
--}


data Patricia   = Undefined 

{--
type mapping = address -> uint 
--}
















{--
contract bank { 

method withdraw (amount : Wei) {

    data Tree  = Leaf amount 
               | Node account Tree Tree 


--}

data IndTree    = ILf ID 
                | ILfTy Ty
                | IBr [IndTree] 
                | IBrAll ID [IndTree] 
                | IBrEx  ID [IndTree] 
