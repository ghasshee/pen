module Analysis where 


-- import Data.Matrix hiding (zero)

import Logic
import Set 
import Mat hiding (getRow, getCol) 
import Node
import Edge
import Action
import Semiring
import OR 
import PG
import GCLL hiding (M) 
import Opcode hiding (OR) 


--import Data.List (sort) 

-- || Matrix Generation Function || -- 

-- matrix :: Int -> Int  -> ( (Int,Int) -> a ) -> Matrix a  
-- matrix   #row   #col     generatorFunction   = result

genSize :: PG -> (Int {--Rows--} , Int {--Cols--} )  
genSize (e, (i,t,q,s,v,ctx,stx)) = (q+1,q+1)

genFun :: PG -> (Int,Int) -> OR (Edge Int Action) 
genFun (e, (_,_,q,s,v,ctx,stx))  (i,j) = 
    case searchEdge e i j of 
        Nothing             -> ZR 
        Just a              -> SQ [(i,a,j)] 

searchEdge :: [Edge(Node Int)Action] -> Int -> Int -> Maybe Action 
searchEdge []               i j                  = Nothing 
searchEdge ((Q n,a,Q m):es) i j | i==n&&j==m     = Just a 
                                | otherwise      = searchEdge es i j

genMat :: PG -> Matrix (OR (Edge Int Action)) 
genMat edges = matrix i j gen
    where   gen   = genFun edges 
            (i,j) = genSize edges 




-- || Operations on Matrix || --

instance Semiring a => Semiring (Matrix a) where
    a <.> b = mult a b 
    a <+> b = undefined 
    zero    = undefined 
    iszero  = undefined 

instance (Eq a, Semiring a) => StarSemiring (Matrix a) where 
    star a = loop a a where 
        loop a an = if an == mult a an then an else loop a (mult a an)





diag a@(M n m _ _ _ _) = [ a ! (i,i) | i <- [1 .. n] ]   





-- || Success Path || -- 
--success a@(M i j _ _ _ _) = a ! (1,j-1) 
success :: Matrix a -> a  
success a@(M i j _ _ _ _) = a ! (1,2)  -- ( init node , terminal node ) 


showListLn []     = ""
showListLn (x:xs) = show x ++ "\n" ++ showListLn xs 


rmActions :: Eq a => OR(Edge Int a) -> Matrix (OR (Edge Int a)) -> Matrix (OR (Edge Int a))
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





succPG i a@(M n m _ _ _ _) = filter (not . iszero) [ a!(i,j) | j <- [1..n] ] 
predPG j a@(M n m _ _ _ _) = filter (not . iszero) [ a!(i,j) | i <- [1..n] ] 

succNodes :: (Eq a, Semiring a) => Int -> Matrix a -> [Int]  
succNodes i a@(M n m _ _ _ _) = filter (\k -> not (iszero $ a!(i,k))) [1..n] 

predNodes :: (Eq a, Semiring a) => Int -> Matrix a -> [Int] 
predNodes j a@(M n m _ _ _ _) = filter (\k -> not (iszero $ a!(k,j))) [1..n] 

confluenceNodes a = loop [1] [] [] where 
    loop []            reached confluences = confluences 
    loop (curr:uncles) reached confluences 
        | curr ∈ reached    = loop                      uncles reached (curr:confluences) 
        | otherwise         = loop (succNodes curr a ++ uncles) (curr:reached) confluences  

bifurcationNodes a = loop [2] [] [] where 
    loop []            reached bifurcations = bifurcations 
    loop (curr:uncles) reached bifurcations 
        | curr ∈ reached    = loop                      uncles  reached (curr:bifurcations)
        | otherwise         = loop (predNodes curr a ++ uncles) (curr:reached) bifurcations

decomposedPaths :: (Ord a, Semiring a) => Matrix a -> [(Int,a,Int)]
decomposedPaths a@(M n m _ _ _ _) = loop a [] n where 
    js = uniq $ [1,2] ++ junctionNodes a ++ terminalNodes a -- ++ initialNodes a 
    loop an n_paths n = if n == 0   
                        then uniq $ concat $ reverse n_paths 
                        else loop an' (paths:n_paths) (n-1) where 
                                paths = filter (not . iszero . arrow) [ (i,an!(i,j),j) | i <- js ,j <- js] 
                                diags = filter (not . iszero . arrow) [ (i,an!(i,i),i) | i <- js ] 
                                an'   = a <.> rmPaths paths an
                                rmPaths pathes a = foldr (\(i,_,j) -> setElem zero (i,j)) a pathes 
                            


getPath :: [(Int,a,Int)] -> (Int,Int) -> [a] 
getPath []           (i,j)                  = []  
getPath ((n,a,m):es) (i,j)  | i==n && j==m  = a : getPath es (i,j) 
                            | otherwise     =     getPath es (i,j) 

paths2mat :: (Eq a, Semiring a) => Int -> [(Int,a,Int)] -> Matrix a 
paths2mat n es = matrix n n gen where 
    gen p = loop paths where 
        paths = getPath es p 
        loop ps = case ps of 
            []     -> zero 
            [a]    -> a 
            (a:as) -> a <+> loop as 

nodeReduction :: (Eq a, Ord a, Semiring a) => Matrix a -> Matrix a 
nodeReduction a@(M n m _ _ _ _) = rmNodes (isolatedNodes a') a' where 
    a' = (paths2mat n (decomposedPaths a)) 


rmNodes :: Eq a => [Int] -> Matrix a -> Matrix a 
rmNodes [] a        = a 
rmNodes (i:is) a    = minorMatrix i i (rmNodes is a)  


junctionNodes a@(M n m _ _ _ _) = filter (moreSuccNode a ||$ morePredNode a) [3..n] 
pathNodes     a@(M n m _ _ _ _) = filter ( oneSuccNode a &&$  onePredNode a) [3..n] 
isolatedNodes a@(M n m _ _ _ _) = filter (\i -> foldr (\k -> (&&) (iszero (a!(i,k)) && iszero (a!(k,i))) ) True [1..n]) [3..n]     
initialNodes  a@(M n m _ _ _ _) = filter (zeroPredNodes a) [3..n] 
terminalNodes a@(M n m _ _ _ _) = filter (zeroSuccNodes a) [3..n] 

moreSuccNode a i    = length (succNodes i a)  > 1 
morePredNode a i    = length (predNodes i a)  > 1 
oneSuccNode  a i    = length (succNodes i a) == 1
onePredNode  a i    = length (predNodes i a) == 1 
zeroSuccNodes a i   = length (succNodes i a) == 0 
zeroPredNodes a i   = length (predNodes i a) == 0 


{--

-- || Loop Entrance Nodes & Confluence Nodes || -- 
loopEntranceNodes :: (Eq a, Semiring a) => Matrix a -> ([Int], [Int])
loopEntranceNodes a = loop 1 [] [[]] [] [] [] where 
  loop curr uncles (ans:ancestors) reached confluences entrances 
    | curr ∈ concat ancestors   = case uncles of 
        []          -> (curr : entrances, confluences ) 
        [u]   :uss  -> loop u uss      ([]:ancestors) (ans ++ reached) confluences (curr : entrances) 
        (u:us):uss  -> loop u (us:uss) ([]:ancestors) (ans ++ reached) confluences (curr : entrances)  
    | curr ∈ reached            = case uncles of 
        []          -> (entrances, curr : confluences ) 
        [u]   :uss  -> loop u uss      ([]:ancestors) (ans ++ reached) (curr : confluences) entrances 
        (u:us):uss  -> loop u (us:uss) ([]:ancestors) (ans ++ reached) (curr : confluences) entrances 
    | otherwise                 = case succNodes curr a of 
        []              -> case uncles of 
            []              -> (entrances, confluences) 
            [u]   :uss      -> loop u uss      ([]:ancestors) (curr:ans ++ reached) confluences entrances
            (u:us):uss      -> loop u (us:uss) ([]:ancestors) (curr:ans ++ reached) confluences entrances 
        [n]             -> loop n uncles      (   (curr:ans):ancestors) reached confluences entrances 
        (n:ns)          -> loop n (ns:uncles) ([]:(curr:ans):ancestors) reached confluences entrances  


--}




lu = luDecomp 










-- After Node Reduction 


----------------------------------------
---  Renode with existing Numbering  ---
----------------------------------------


startNode :: OR (Edge Int Action) -> Int 
startNode ZR = -1 
startNode (SQ ((i,_,_):_)) = i 
startNode (OR a b)  | startNode a == startNode b    = startNode a 
                    | otherwise                     = error "edge start ambiguous"  

endNode :: OR (Edge Int Action) -> Int 
endNode ZR             = -1 
endNode (SQ [])        = error "Node reduction has not been done in a good way" 
endNode (SQ [(_,_,j)]) = j 
endNode (SQ (e:es))    = endNode (SQ es) 
endNode (OR a b)    | endNode a == endNode b    = endNode b 
                    | otherwise                 = error "edge end ambiguous" 


innerizeOR :: OR (Edge Int Action) -> Edge Int (OR Action)
innerizeOR or = (startNode or, fmap arrow or, endNode or) 


---------------------------------------
---  Renode with New Numbering      ---
---------------------------------------

reNodeMat :: Matrix (OR Action) -> Matrix (Edge Int (OR Action))
reNodeMat a@(M n _ _ _ _ _) = matrix n n (\(i,j) -> (i, a!(i,j) , j)) 


