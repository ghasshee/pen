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

genSize :: Edges -> (Int {--Rows--} , Int {--Cols--} )  
genSize (e, (i,t,q,s,v,ctx,stx)) = (q+1,q+1)

genFun :: Edges -> (Int,Int) -> OR (Edge Int Action) 
genFun (e, (_,_,q,s,v,ctx,stx))  (i,j) = 
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





succEdges i a@(M n m _ _ _ _) = filter (not . iszero) [ a!(i,j) | j <- [1..n] ] 
predEdges j a@(M n m _ _ _ _) = filter (not . iszero) [ a!(i,j) | i <- [1..n] ] 

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
-- Branching Point Analysis


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



----------------------
--- Branching      ---
----------------------

data Branch a   = BIf Int a [a] [a] Int Int   
                | BDp Int [(a, Int, [a], Int)] 
                | BCk Int [(a, Int, [a], Int)]  
                | BSq Int [a] Int 
                | BZr


branch2opcode :: Branch Action -> [OPCODE] 
branch2opcode  = undefined 


instance Show a => Show (Branch a) where 
    show (BIf i c as bs j k) = "(" ++ show i ++ ",IF " ++ show c ++ " THEN " ++ show as ++ " ELSE " ++ show bs ++ ", (" ++ show j ++ "," ++ show k ++ "))"
    show (BDp i dsps)        = "(" ++ show i ++ ", [" ++ showChks dsps ++ ")" where 
        showChks []                     = "]"
        showChks ((a, q, as, j):chks)      = "(IF " ++ show a ++ " THEN[" ++ show q ++ "] " ++ show as ++ ", " ++ show j ++ ") " ++ showChks chks    
    show (BCk i chks)        = "(" ++ show i ++ ", [" ++ showChks chks ++ ")" where 
        showChks []                     = "]"
        showChks ((a, q, as, j):chks)      = "(IF " ++ show a ++ " THEN[" ++ show q ++ "]" ++ show as ++ ", " ++ show j ++ ") " ++ showChks chks    
    show (BSq i as j)        = "(" ++ show i ++ ", " ++ show as ++ ", " ++ show j ++ ")"
    show (BZr)               = "_" 





----------------------------------------------
---   Matrix -> [[Edge Int (OR Action)]]   ---
----------------------------------------------


rows :: Semiring a => Matrix a -> [[a]] 
rows a@(M n _ _ _ _ _) = [ [ a!(i,j) | j <- [1..n] , (not . iszero) (a!(i,j)) ] | i <- [1..n] ]  



------------------------------------
---  [[OR Action]]  OR Removal   ---  
------------------------------------

decomposeOR :: OR a -> [OR a] 
decomposeOR (OR a b) = decomposeOR a ++ decomposeOR b  
decomposeOR (SQ s)   = [SQ s] 
decomposeOR ZR       = []  

decompEdgeOR :: Edge node (OR a) -> [Edge node (OR a)] 
decompEdgeOR (i,a,j) = map (\a -> (i,a,j)) (decomposeOR a)  


putoffOR :: OR a -> [a]
putoffOR (SQ s)             = s 
putoffOR _                  = error "putoffOR: unexpected Argument" 

putoffEdgeOR :: Edge node (OR a) -> Edge node [a] 
putoffEdgeOR (i,SQ s, j)    = (i,s,j) 
putoffEdgeOR _              = error "putoffEdgeOR: unexpected Argument" 

removeOR :: [OR a] -> [[a]]
removeOR row = concat $ map ( map putoffOR . decomposeOR ) row 

removeEdgeOR :: [Edge node (OR a)] -> [Edge node [a]]
removeEdgeOR row = concat $ map (map putoffEdgeOR . decompEdgeOR) row



-----------------------------------
---      [[[Action]]]           ---
-----------------------------------

-- data Structure OR removed 
-- so after removeOR adaption, the data is e.g. like this    
--
--  [
--  [[a1,a2], [cond, a3,a4]] , 
--  [[chk1, a1, ..], [chk2, a2, ..], ..] , 
--  [[dsptch1, a1, ..], [dsptch2, a2, ..], ..] , 
--  ] 
--
--
-- Now we find the branching condition from the list, and 
-- reordering so that the actions with cond is the head 


-- findCond :: NewNode -> [Edge Int [Action]] -> (NewNode, Branch Action) 
findCond :: Int -> [Edge Int [Action]] -> (Int, Branch Action) 
findCond n [(i,a:as,j),(i',AcSkip:as',j')] 
    | isCond a  && i == i' = (n, BIf i a as as' j j' ) 
findCond n [(i,AcSkip:as,j),(i',a':as',j')] 
    | isCond a' && i == i' = (n, BIf i a' as' as j' j )
findCond n ((i,a:as,j):es) | isDspCond a = (n', BDp i ((a, n, as, j):es')) where 
    (n', es') = loop (n+1) es
    loop n []                           = (n,  [])  
    loop n ((i', (a':as'), j'):es') 
        | isDspCond a' && i == i'           = (n', (a', n, as', j'): es'') 
        where 
            (n', es'') = loop (n+1) es' 
findCond n ((i,(a:as),j):es) | isCheckCond a = (n', BCk i ((a, n, as, j): es'))  where 
    (n', es') = loop (n+1) es 
    loop n []                           = (n, [])  
    loop n ((i', (a':as'), j'):es') 
        | isCheckCond a' && i == i'         = (n', (a',n, as', j'): es'')    
        where 
            (n', es'') = loop (n+1) es' 
findCond n [(i,as,j)]   = (n, BSq i as j) 
findCond n []           = (n, BZr) 
findCond n e            = error (show e) 


branching :: [[Edge Int [Action]]] -> [Branch Action] 
branching ess = bs where 
    (n', bs)    = loop n ess 
    n           = length ess + 1
    loop n []       = (n, [])
    loop n (es:ess) = (n'', b:bs) where 
        (n'', bs)   = loop n' ess
        (n', b)     = findCond n es 


branch :: Matrix (Edge Int (OR Action)) -> [Branch Action] 
branch = branching . map removeEdgeOR . rows  

---------------------------------
--- [ Branch Action ]         ---
---------------------------------

