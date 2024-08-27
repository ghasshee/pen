module MatRep where 


-- import Data.Matrix hiding (zero)

import Set 
import Mat hiding (getRow, getCol) 
import Node
import Edge
import Action
import Semiring
import OR 
import PG
import GCLL hiding (M) 



-- || Matrix Generation Function || -- 

-- matrix :: Int -> Int  -> ( (Int,Int) -> a ) -> Matrix a  
-- matrix   #row   #col     generatorFunction   = result

genSize :: Edges -> (Int {--Rows--} , Int {--Cols--} )  
genSize (e, (i,t,q,s,v,ctx)) = (q+1,q+1)

genFun :: Edges -> (Int,Int) -> OR (Edge Int Action) 
genFun (e, (_,_,q,s,v,ctx))  (i,j) = 
    case searchEdge e i j of 
        Nothing             -> ZR 
        Just a              -> SQ [(i,a,j)] 

searchEdge :: [Edge(Node Int)Action] -> Int -> Int -> Maybe Action 
searchEdge []               i j                  = Nothing 
searchEdge ((Q n,a,Q m):es) i j | i==n&&j==m     = Just a 
                                | otherwise      = searchEdge es i j

genMat :: Edges -> Matrix (OR (Edge Int Action)) 
genMat edges = matrix i j gen
    where   gen   = genFun edges 
            (i,j) = genSize edges 


-- || Operations on Matrix || --


star :: (Semiring a,Eq a) => Matrix a -> Matrix a 
star a = loop a a where 
    loop a an = if an == mult a an then an else loop a (mult a an)





diag a@(M n m _ _ _ _) = [ a ! (i,i) | i <- [1 .. n] ]   





-- || Success Path || -- 
--success a@(M i j _ _ _ _) = a ! (1,j-1) 
success :: Matrix a -> a  
success a@(M i j _ _ _ _) = a ! (1,2)  -- ( init node , terminal node ) 


showListLn []     = ""
showListLn (x:xs) = show x ++ "\n" ++ showListLn xs 


rmActions :: OR(Edge Int a) -> Matrix (OR (Edge Int a)) -> Matrix (OR (Edge Int a))
rmActions (OR t t')          a  = rmActions t' (rmActions t a) 
rmActions (SQ [])            a  = a 
rmActions (SQ ((i,ac,j):xs)) a  = rmActions (SQ xs) (setElem zero (i,j) a)
rmActions ZR                 a  = a 


rmLoops :: Eq a => Matrix (OR (Edge Int a)) -> Matrix (OR (Edge Int a)) -> Matrix (OR (Edge Int a))
rmLoops an a = 
    let d       = diag an in 
    let trs     = foldr (\x xs -> if x == ZR then xs else x:xs) [] d in 
    let a'      = foldr rmActions a trs in 
    a' 


star' a@(M n _ _ _ _ _) = loop a a n [] where 
    loop a an limit ans  
        | elem (mult a an) ans  = an
        | otherwise             = loop a' (mult a' an) (limit-1) (an:ans) where 
                                        a' = rmLoops an a 


convert :: Matrix (OR (Edge Int Action)) -> Matrix (OR Action) 
convert = fmap (fmap arrow) 

-------------------------------
----  Graph Loop Analysis  ----
-------------------------------





getCol j a@(M n m _ _ _ _) = [ a!(i,j) | i <- [1..n] ]
getRow i a@(M n m _ _ _ _) = [ a!(i,j) | j <- [1..n] ] 


nextNodes :: (Eq a, Semiring a) => Int -> Matrix a -> [Int]  
nextNodes i a = loop (getRow i a) 1 where 
    loop []     _                   = [] 
    loop (x:xs) i   | x == zero     =     loop xs (i+1) 
                    | otherwise     = i : loop xs (i+1)  


-- || Loop Entrance Nodes & Confluence Nodes || -- 
loopEntranceNodes :: (Eq a, Semiring a) => Matrix a -> ([Int], [Int])
loopEntranceNodes a = loop 1 [] [[]] [] [] [] where 
  loop curr uncles (ans:ancesters) reached confluences entrances 
    | curr ∈ concat ancesters   = case uncles of 
        []          -> (curr : entrances, confluences ) 
        [u]   :uss  -> loop u uss      ([]:ancesters) (ans ++ reached) confluences (curr : entrances) 
        (u:us):uss  -> loop u (us:uss) ([]:ancesters) (ans ++ reached) confluences (curr : entrances)  
    | curr ∈ reached            = case uncles of 
        []          -> (entrances, curr : confluences ) 
        [u]   :uss  -> loop u uss      ([]:ancesters) (ans ++ reached) (curr : confluences) entrances 
        (u:us):uss  -> loop u (us:uss) ([]:ancesters) (ans ++ reached) (curr : confluences) entrances 
    | otherwise                 = case nextNodes curr a of 
        []              -> case uncles of 
            []              -> (entrances, confluences) 
            [u]   :uss      -> loop u uss      ([]:ancesters) (curr:ans ++ reached) confluences entrances
            (u:us):uss      -> loop u (us:uss) ([]:ancesters) (curr:ans ++ reached) confluences entrances 
        [n]             -> loop n uncles      (   (curr:ans):ancesters) reached confluences entrances 
        (n:ns)          -> loop n (ns:uncles) ([]:(curr:ans):ancesters) reached confluences entrances  





