module Datatype where 

import Hex
import Type 
import LTree
import Utils 





-- Inductive Data Type 
-- 
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


dconstr2ltree (DConstr id ty) = LBr [LLf id, LBr [LLf . show $ ty]] 

dind2ltree (DInd id args ty constrs) = LBr (LLf id : LBr (LLf <$> args) : [LBr (dconstr2ltree <$> constrs)])



instance Show DTy where 
    show (DNew   s)  = s 
    show (DVar   s)  = s 
    show (DTy   ty)  = show ty
    show (DProd l)   = "(" ++ showProd l ++ ")" where 
        showProd []  = ""
        showProd [t] = show t
        showProd (t:ts) = show t ++ "," ++ showProd ts 


------------------------------------
---     memorize with FIX        ---
------------------------------------
    

data Fix f = In (f (Fix f))

data ListF a x = NilF | ConsF a x 
    deriving (Show)

type List a = Fix (ListF a) 

a = In (ConsF 1 (In (ConsF 2 (In NilF))))

toList (In NilF) = [] 
toList (In (ConsF a as)) = a : toList as 



    {--
# solc 

# mapping 
# variable 
--}


data Patricia   = Empty 
                | Leaf [Integer] Int 
                | Node [Integer] Patricia Patricia 

{--
type mapping = address -> uint 
--}








data D x = DN
         | DC 
         | DP x x 

type DD = Fix D  
        
toD (DNIL) = DN 
toD (DCONST) = DC
toD (DPAIR d e) = undefined 











------------------------------------
--- Encode with  cantor Pairing  ---
------------------------------------

i_sqrt 0 = 0 
i_sqrt 1 = 1 
i_sqrt n = hd $ dropWhile (not . isRoot) iters where 
    (_R, _N) = last $ takeWhile ((n>=) . snd) $ zip (1:pow2s) pow2s 
    newton x = (x + n `div` x) `div` 2
    iters = iterate newton (i_sqrt (n `div` _N) * _R)
    isRoot r = r ^ 2 <= n && n < (r+1) ^ 2 
    pow2s  = iterate (^2) 2   


cantorPair :: (Integer, Integer) -> Integer 
cantorPair (x,y) = (x + y) * (x + y + 1) `div` 2 + y  

cantorUnpair :: Integer -> (Integer,Integer) 
cantorUnpair z = (w-y, y) where 
    y = z - t
    t = w * (w+1) `div` 2
    w = (i_sqrt (8 * z + 1) - 1) `div` 2



data DT     = DNIL
            | DCONST  
            | DPAIR DT DT 
            
            deriving (Show, Eq, Read) 

encodeDT  :: DT -> Integer 
encodeDT  DNIL                  = 0 
encodeDT  DCONST                = 1     
encodeDT  (DPAIR d e)         = cantorPair (encodeDT d, encodeDT e)


decodeDT  :: Integer -> DT 
decodeDT 0 = DNIL
decodeDT 1 = DCONST
decodeDT n = 
    let (i,j) = cantorUnpair n in 
    DPAIR (decodeDT i)(decodeDT j) 

        
depth :: DT -> Integer
depth DNIL = 0
depth DCONST = 1
depth (DPAIR d e) = 1 + max (depth d) (depth e) 


leng :: DT -> Integer 
leng DNIL = 0 
leng DCONST = 1
leng (DPAIR d e) = leng d + leng e 


balanceBinTree = undefined 

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
