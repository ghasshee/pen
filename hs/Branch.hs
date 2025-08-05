
module Branch where 

import OR 
import Mat
import Edge 
import Semiring 
import Opcode hiding (OR) 
import Action 
import Analysis

import Action2Opcode

----------------------
--- Branching      ---
----------------------

data Branch a   = BIf Int a Int [a] [a] Int Int   
                | BDp Int [(a, Int, [a], Int)] 
                | BCk Int [(a, Int, [a], Int)]  
                | BSq Int [a] Int 
                | BZr




instance Show a => Show (Branch a) where 
    show (BIf i c q as bs j k) = "(" ++ show i ++ ",IF " ++ show c ++ " THEN{" ++ show q ++ "} " ++ show as ++ " ELSE " ++ show bs ++ ", (" ++ show j ++ "," ++ show k ++ "))"
    show (BDp i dsps)        = "(" ++ show i ++ ", [" ++ showChks dsps ++ ")" where 
        showChks []                     = "]"
        showChks ((a, q, as, j):chks)      = "(IF " ++ show a ++ " THEN{" ++ show q ++ "} " ++ show as ++ ", " ++ show j ++ ") " ++ showChks chks    
    show (BCk i chks)        = "(" ++ show i ++ ", [" ++ showChks chks ++ ")" where 
        showChks []                     = "]"
        showChks ((a, q, as, j):chks)      = "(IF " ++ show a ++ " THEN{" ++ show q ++ "}" ++ show as ++ ", " ++ show j ++ ") " ++ showChks chks    
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
---   [[Edge Int [Action]]]     ---
-----------------------------------

-- After removeOR adaption, the data is e.g. like this    
--
--  [
--  [[a1,a2], [cond, a3,a4]] , 
--  [[chk1, a1, ..], [chk2, a2, ..], ..] , 
--  [[dsptch1, a1, ..], [dsptch2, a2, ..], ..] , 
--  ] 
--
-- Now we pick up the branching condition from the list, 
-- and transform them into data Branch. 



-- | :: NewNode -> [Edge Int [Action]] -> (NewNode, Branch Action) 
findCond :: Int -> [Edge Int [Action]] -> (Int, Branch Action) 
findCond n [(i,a:as,j),(i',AcSkip:as',j')] 
    | isCond a  && i == i' = (n+1, BIf i a n as as' j j' ) 
findCond n [(i,AcSkip:as,j),(i',a':as',j')] 
    | isCond a' && i == i' = (n+1, BIf i a' n as' as j' j )
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
findCond n e            = error $ "findCond: [Unexpected Action Seq] " ++ show e   


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

