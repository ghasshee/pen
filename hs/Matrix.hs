module Matrix where 


-- import Data.Matrix hiding (zero)

import Mat hiding (getRow) 
import Action
import Semiring
import PG
import GCLL hiding (M) 


-- matrix :: Int -> Int  -> ( (Int,Int) -> a ) -> Matrix a  
-- matrix   #row   #col     generatorFunction   = result

genSize :: Edges -> (Int {--Rows--} , Int {--Cols--} )  
genSize (e, (i,t,q,s,v,ctx)) = (q+1,q+1)

genFun :: Edges -> (Int,Int) -> OR Transition
genFun (e, (_,_,q,s,v,ctx))  (i,j) = 
    let matsize = q in 
    case searchEdge e i j matsize of 
        Nothing             -> ZR 
        Just a              -> SQ [Tr(i,a,j)] 

searchEdge :: [Edge] -> Int -> Int -> Int -> Maybe Action 
searchEdge []               i j sz                  = Nothing 
searchEdge ((Qi, a,Qt ):es) 0 j sz | j==sz          = Just a 
searchEdge ((Qi, a,Q n):es) 0 j sz | j==n           = Just a
searchEdge ((Q n,a,Qt ):es) i j sz | i==n&&j==sz    = Just a 
searchEdge ((Q n,a,Q m):es) i j sz | i==n&&j==m     = Just a 
searchEdge (e          :es) i j sz                  = searchEdge es i j sz


genMat :: Edges -> Matrix (OR Transition) 
genMat edges = matrix i j gen
    where   gen   = genFun edges 
            (i,j) = genSize edges 




star :: (Semiring a,Eq a) => Matrix a -> Matrix a 
star a = loop a a where 
    loop a an = if an == mult a an then an else loop a (mult a an)

success a@(M i j _ _ _ _) = a ! (1,j-1) 

showListLn []     = ""
showListLn (x:xs) = show x ++ "\n" ++ showListLn xs 

diag a@(M n m _ _ _ _) = [ a ! (i,i) | i <- [1 .. n] ]   

removeTrsFrom a (OR t t')               = removeTrsFrom (removeTrsFrom a t) t' 
removeTrsFrom a (SQ [])                 = a 
removeTrsFrom a (SQ (Tr(i,ac,j):xs))    = removeTrsFrom (setElem ZR (i,j) a) (SQ xs)
removeTrsFrom a ZR                      = a 

removeLoops an a = 
    let d       = diag an in 
    let trs     = foldl (\xs x -> if x == ZR then xs else x:xs) [] d in 
    let a'      = foldl (\xs x -> removeTrsFrom xs x) a trs in 
    a' 

star' a@(M n _ _ _ _ _) = loop a a n [] where 
    loop a an limit ans  
        | elem (mult a an) ans          = an
        | otherwise                     = loop a' (mult a' an) (limit-1) (an:ans) where 
            a' = removeLoops an a 


convert :: Matrix (OR Transition) -> Matrix (OR Action) 
convert = fmap (fmap tr2ac) 

-------------------------------
----  Graph Loop Analysis  ----
-------------------------------


type LoopEntranceNode = Int

data Path a     = Path Int Int a 
                | Loop Int LoopEntranceNode a 
                | None 
                

getPaths = undefined 
