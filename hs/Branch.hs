
module Branch where 

import OR 
import Mat
import Node 
import Edge hiding (partition) 
import Config
import Semiring 
import Set
import Logic 
import Opcode hiding (OR) 
import Action 
import Analysis
import Action2Opcode
import Utils 

import Data.List (partition)
----------------------
--- Branching      ---
----------------------

data Branch a   = BIf  Int [a] Int [a] [a] Int Int   
                | BIf' Int ([a],Int,Int,[a]) Int [a] [a] Int Int  
                | BDp  Int [(a, Int, [a], Int)] 
                | BCk  Int [(a, Int, [a], Int)]  
                | BSq  Int [a] Int 
                | BZr


instance Show a => Show (Branch a) where 
    show (BIf' i c q as bs j k) = "(" ++ show i ++ ",IF " ++ show c ++ " THEN{" ++ show q ++ "} " ++ show as ++ " ELSE " ++ show bs ++ ", (" ++ show j ++ "," ++ show k ++ "))"
    show (BIf i c q as bs j k) = "(" ++ show i ++ ",IF " ++ show c ++ " THEN{" ++ show q ++ "} " ++ show as ++ " ELSE " ++ show bs ++ ", (" ++ show j ++ "," ++ show k ++ "))"
    show (BDp i dsps)        = "(" ++ show i ++ ", [" ++ showChks dsps ++ ")" where 
        showChks []                     = "]"
        showChks ((a, q, as, j):chks)      = "(IF " ++ show a ++ " THEN{" ++ show q ++ "} " ++ show as ++ ", " ++ show j ++ ") " ++ showChks chks    
    show (BCk i chks)        = "(" ++ show i ++ ", [" ++ showChks chks ++ ")" where 
        showChks []                     = "]"
        showChks ((a, q, as, j):chks)      = "(IF " ++ show a ++ " THEN{" ++ show q ++ "}" ++ show as ++ ", " ++ show j ++ ") " ++ showChks chks    
    show (BSq i as j)        = "(" ++ show i ++ ", " ++ show as ++ ", " ++ show j ++ ")"
    show (BZr)               = "_" 


instance {-# Overlapping #-} Show a => Show [Branch a] where 
    show [] = "[]"
    show [b]    = show b ++ "]" 
    show (b:bs) = "[" ++ show b ++ "\n" ++ 
                  "," ++ show bs



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




partCond :: [Action] -> ([Action],[Action]) 
partCond as = loop1 as ([],[]) where 
    loop2 []          (bs,es) = (rev bs, rev es) 
    loop2 (a:as)      (bs,es) = loop2 as (bs    ,  a:es)
    loop1 (AcBool e:as)    _  = ([AcBool e], as)
    loop1 (AcDonc:as) (bs,es) = loop2 as (AcDonc:bs, es)
    loop1 (a:as)      (bs,es) = loop1 as (a     :bs, es)

findCond :: FreshNode -> [Edge Int [Action]] -> (FreshNode, Branch Action) 
findCond q [(i,a:as,j), (i',AcSkip:as',j')] | isCond a  && i==i'    = (q+1, BIf i bs  q es as' j j' ) where 
    (bs,es) = partCond (a:as) 
findCond q [(i,AcSkip:as,j),(i',a':as',j')] | isCond a' && i==i'    = (q+1, BIf i bs q es as j' j ) where
    (bs,es) = partCond (a':as') 
findCond q ((i,a:as,j):es)                  | isDsp  a              = (n' , BDp i ((a, q, as, j):es')) where 
    (n', es')                                                       = loop (q+1) es
    loop n []                                                       = (n,  [])  
    loop n ((i',a':as',j'):es') | isDsp a'   && i==i'               = (n', (a',n,as',j'):es'') 
                                | a'==AcSkip && i==i'               = loop  n   (es'++[(i',a':as',j')]) where 
            (n', es'')                                              = loop (n+1) es' 
findCond n ((i,a:as,j):es)      | isChk a                           = (n', BCk i ((a, n, as, j): es'))  where 
    (n', es')                                                       = loop (n+1) es 
    loop n []                                                       = (n, [])  
    loop n ((i',a':as',j'):es') | (isChk a' && i==i') || a'==AcSkip = (n', (a',n, as', j'): es'') where  (n', es'') = loop (n+1) es' 
findCond n ((i,a:as,j):es)      | a==AcSkip                         = (n', BDp i ((a, n, as, j): es'))  where 
    (n', es')                                                       = loop (n+1) es 
    loop n []                                                       = (n, [])  
    loop n ((i',a':as',j'):es') | (isDsp a' && i==i')               = (n', (a',n, as', j'): es'') where  (n', es'') = loop (n+1) es' 
        
findCond n [(i,as,j)]                                               = (n, BSq i as j)  
findCond n []                                                       = (n, BZr) 
findCond n e                                                        = error $ "findCond: [Unexpected Action Seq] " ++ show e   


branching :: [[Edge Int [Action]]] -> [Branch Action] 
branching _ess = br ++ bs where 
    (ess,div)   = undivide _ess 
    q           = length ess + 1
    (q'',br)    = branchdivides q' div
    (q', bs)    = loop q ess 
    loop q []       = (q, [])
    loop q (es:ess) = (q'', b:bs) where 
        (q', b)     = findCond q es 
        (q'', bs)   = loop q' ess


branchdivides :: Int -> [[Edge Int [Action]]] -> (Int, [Branch Action]) 
branchdivides q []                       = (q, []) 
branchdivides q (ifthenelse:ifthenelses) = (q'',(b:bs)) where 
    (q',b)   = branchdivide  q  ifthenelse 
    (q'',bs) = branchdivides q' ifthenelses
branchdivide :: Int -> [Edge Int [Action]] -> (Int, Branch Action) 
branchdivide q [(i,cond,k),(l,doncval,j),(_,_else,j')] = 
    let (donc, _then) = partCond doncval in  
    (q, BIf' i (cond,k,l,donc) q _then _else j j') 

undivide :: [[Edge Int [Action]]] -> ([[Edge Int [Action]]], [[Edge Int [Action]]])
undivide ess = loop ess [] [] where 
    loop []                         ret div                  = (ret,div)  
    loop (es:ess) ret div = case accond es of 
        Just ((i,AcCond:as,j), es) | lastrecord as -> loop (ess') (es':ret) (ifthenelse:div) where 
            ifthenelse              =   (i,AcCond:as,j) : (j', donc ,k) : es 
            ([(j',donc,k)],es')     =   partition (\e -> AcCheck qi qt `elem` arrow e ) $ hd line 
            (line, ess')            =   partition (accheck qi qt) (ret ++ ess) 
            AcRecord qi qt          =   last as  
        _                                          -> loop ess (es:ret) div 


accheck ::  Node Int  -> Node Int  -> [Edge Int [Action]] -> Bool 
accheck i t [] = False 
accheck i t (e:es) = (AcCheck i t `elem` as ) || accheck i t es 
    where  as = arrow e 
     


accond es = loop es [] where 
    loop []                _        = Nothing 
    loop (e@(_,AcCond:_,_):es) es'  = Just (e, es++es')  
    loop (e:es) es'                 = return . ( ((:)e) <$> ) =<< loop es es' 

lastrecord as = case last as of 
    AcRecord _ _    -> True
    _               -> False 


branch :: Matrix (Edge Int (OR Action)) -> [Branch Action] 
branch = branching . map removeEdgeOR . rows  



