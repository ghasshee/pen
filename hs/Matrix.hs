module Matrix where 


-- import Data.Matrix hiding (zero)

import Mat 
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
        Nothing             -> zro 
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




star :: (Semiring a,Eq a) => Matrix a -> Matrix a 
star a = loop a a where 
    loop a an = if an == mult a an then an else loop a (mult a an)

finstar :: (Semiring a) => Int -> Matrix a -> Matrix a 
finstar k a = loop k where 
    loop 1 = a 
    loop n = mult a (loop (n-1))
    
-- extract Success Path 
--success a@(M i j _ _ _ _) = star a ! (1,j) 
success a@(M i j _ _ _ _) = finstar 100 a ! (1,j-1) 
--success a@(M i j _ _ _ _) = [ mul a ! (1,k) | k <- [1..j] ]  

showListLn []     = ""
showListLn (x:xs) = show x ++ "\n" ++ showListLn xs 











-------------------------------
----  Graph Loop Analysis  ----
-------------------------------



diag a@(M n m _ _ _ _) = [ a ! (i,i) | i <- [0 .. n-1] ]   

hasLoop :: (Semiring a, Eq a) => Matrix a -> Bool 
hasLoop a = foldl (\xs x -> x == zro && xs) True (diag a) 

loopsAt :: (Semiring a, Eq a) => Matrix a -> Maybe Int
loopsAt a@(M n m _ _ _ _) = loop (diag a) where 
    loop []     = Nothing 
    loop (d:ds) = if d /= zro then Just (n - length (d:ds)) else loop ds 


analyze a = loop a a where 
    loop a an = case loopsAt an of 
        Nothing -> loop a (mult a an) 
        Just i  -> undefined 




