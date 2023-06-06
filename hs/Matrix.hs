module Matrix where 


import Data.Matrix hiding (zero)

import Action
import Semiring
import PG


-- matrix :: Int -> Int  -> ( (Int,Int) -> a ) -> Matrix a  
-- matrix   #row   #col     generatorFunction   = result

genSize :: Edges -> (Int {--Rows--} , Int {--Cols--} )  
genSize (e, (i,t,q,s,v,ctx)) = (q+1,q+1)

genFun :: Edges -> (Int,Int) -> OR Action 
genFun (e, (_,_,q,s,v,ctx))  (i,j) = 
    let matsize = q in 
    case searchEdge e i j matsize of 
        Nothing             -> zero 
        Just a              -> LI [a] 

searchEdge []               i j sz                  = Nothing 
searchEdge ((Qi, a,Qt ):es) 0 j sz | j==sz          = Just a 
searchEdge ((Qi, a,Q n):es) 0 j sz | j==n           = Just a
searchEdge ((Q n,a,Qt ):es) i j sz | i==n&&j==sz    = Just a 
searchEdge ((Q n,a,Q m):es) i j sz | i==n&&j==m     = Just a 
searchEdge (e          :es) i j sz                  = searchEdge es i j sz


genMat :: Edges -> Matrix (OR Action) 
genMat edges = matrix i j gen 
    where   gen   = genFun edges 
            (i,j) = genSize edges 




