module Datatype where 

import Type 


data DInd       = DInd ID [ID] DTy [DConstr] 


data DConstr    = DConstr ID DTy  
                deriving (Show, Eq, Read) 


data DTy        = DNew  ID   -- User defined Inductive Data Type (DInd) 
                | DVar  ID
                | DTy   Ty 
                | DArr  [DTy] 
                | DProd [DTy] 
                | DAll  ID DTy [DTy] 
                | DEx   ID DTy [DTy] 
                deriving (Eq, Read) 



instance Show DTy where 
    show (DNew   s)  = s 
    show (DVar   s)  = s 
    show (DTy   ty)  = show ty
    show (DProd l)   = "(" ++ showProd l ++ ")" where 
        showProd []  = ""
        showProd [t] = show t
        showProd (t:ts) = show t ++ "," ++ showProd ts 



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

dty2tree (DNew id)      = ILf   id 
dty2tree (DVar id)      = ILf   id 
dty2tree (DTy  ty)      = ILfTy ty
dty2tree (DArr  l)      = IBr   (dty2forest l) 
dty2tree (DProd l)      = undefined 
dty2tree (DAll id ty l) = IBrAll id (dty2forest l) 
dty2tree (DEx  id ty l) = IBrEx  id (dty2forest l) 

dty2forest []           = [] 
dty2forest (d:ds)       = dty2tree d : dty2forest ds 

dconstr2tree (DConstr id dty) = undefined 

dInd2tree = undefined  
