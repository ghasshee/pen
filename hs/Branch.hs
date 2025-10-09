
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

import GHC.Exts  (sortWith) 
import Data.List (partition) 
----------------------
--- Branching      ---
----------------------

data Branch a   = BIf   Int [a] Int [a] [a] Int Int   
                | BIf'  Int ([a],Int,Int,[a]) Int [a] [a] Int Int  
                | BIf'' Int ([a],Int,[(Int,[a],Int)],Int,[a]) Int [a] [a] Int Int 
                | BDp   Int [(a, Int, [a], Int)] 
                | BCk   Int [(a, Int, [a], Int)]  
                | BSq   Int [a] Int 
                | BZr


instance Show a => Show (Branch a) where 
    show (BIf' i c q as bs j k) = "(" ++ show i ++ ",IF' " ++ show c ++ " THEN{" ++ show q ++ "} " ++ show as ++ " ELSE " ++ show bs ++ ", (" ++ show j ++ "," ++ show k ++ "))"
    show (BIf'' i c q as bs j k) = "(" ++ show i ++ ",IF'' " ++ show c ++ " THEN{" ++ show q ++ "} " ++ show as ++ " ELSE " ++ show bs ++ ", (" ++ show j ++ "," ++ show k ++ "))"
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
--  [[cond, a3, a4, rec5], [a1,a2]] 
--  ] 
--
-- Now we pick up the branching condition from the list, 
-- and transform them into data Branch. 


type Edg    = Edge Int [Action] 
type Edgs   = [Edg] 
type Ac     = Action 
type Br     = Branch 
type Fresh  = FreshNode 

sortEdgs :: Edgs -> Edgs
sortEdgs = rev . sortWith (\(_,a:as,_) -> a) 


partCond :: [Ac] -> ([Ac],[Ac]) 
partCond as = loop1 as ([],[]) where 
    loop2 []          (bs,es) = (rev bs, rev es) 
    loop2 (a:as)      (bs,es) = loop2 as (bs    ,  a:es)
    loop1 (AcDonc:as) (bs,es) = loop2 as (AcDonc:bs, es)
    loop1 (a:as)      (bs,es) = loop1 as (a     :bs, es)
    loop1 []          (bs,es) = (rev bs, rev es) 


edgs2brs :: Fresh -> [Edgs] -> (Fresh, [Br Ac])
edgs2brs q [] = (q,[]) 
edgs2brs q (es:ess) = (q'', b:bs) where 
    (q', b)     = edg2br q (sortEdgs es) 
    (q'', bs)   = edgs2brs q' ess

edg2br :: Fresh -> [Edg] -> (Fresh, Br Ac) 
edg2br q ((i,a:as,j):es) | isCnd a      = (q', BIf i bs q es' as' j j') where 
    (bs, es' )              = partCond (a:as) 
    (q',[(_,_,as',j')])     = edg2br_cont (q+1) es isCnd
edg2br q ((i,a:as,j):es) | isDsp a      = (q', BDp i ((a,q,as,j):brs)) where 
    (q',brs)                = edg2br_cont (q+1) es isDsp 
edg2br q ((i,a:as,j):es) | isChk a      = (q', BCk i ((a,q,as,j):brs)) where 
    (q',brs)                = edg2br_cont (q+1) es isChk 
edg2br q [(i,as,j)]                     = (q, BSq i as j)  
edg2br q []                             = (q, BZr) 
edg2br q e                              = error $ "edg2br: [Unexpected Action Seq] " ++ show e   

edg2br_cont :: Fresh -> [Edg] -> (Ac -> Bool) -> (Fresh, [(Ac, Fresh, [Ac], Fresh)]) 
edg2br_cont q es pred      =   loop q es where 
        loop q []                               =   (q, []) 
        loop q [(i',a':as',j')]     | isSkp a'  =   (q+1, [(a',q,as',j')]) 
        loop q ((i',a':as',j'):es') | pred  a'  =   (q' ,  (a',q,as',j'):brs) where 
            (q', brs)                           =   loop (q+1) es' 


branching :: [Edgs] -> [Br Ac] 
branching ess = br ++ bs where 
    (spliced,ess')  = splice ess 
    q               = length ess + 1
    (q', bs)        = edgs2brs q ess' 
    (q'',br)        = branch_splices q' spliced

branch_splices :: Int -> [Edgs] -> (Int, [Br Ac]) 
branch_splices q []                       = (q, []) 
branch_splices q (cond:conds) = (q'',(b:bs)) where 
    (q',b)   = branch_splice  q  cond 
    (q'',bs) = branch_splices q' conds

branch_splice :: Int -> Edgs -> (Int, Br Ac) 
branch_splice q [(i,cond,k),(l,donc_then,j),(_,_else,j')] = 
    (q, BIf' i (cond,k,l,donc) q _then _else j j') where 
        (donc, _then) = partCond donc_then 
branch_splice q ((i,cond,k):es) = 
    (q, BIf'' i (cond,k,init es,l, donc) q _then _else j j') where 
        (donc, _then) = partCond donc_then
        (_,_else,j')  = last es 
        (l,donc_then,j) = last $ init es 

splice :: [Edgs] -> ([Edgs], [Edgs])
splice ess = loop ess ([],[]) where 
    loop []       (div,ret) = (div,ret)  
    loop (es:ess) (div,ret) = case getAcCond es of 
        ([],[],_)                   -> loop ess (div, es:ret) 
        ([cond],[AcRecord i t],els) -> loop ess' (econd:div,ret) where
            econd               = cond : doncs ++ els
            (doncs,ess')       = getAcChecks i t (ret++ess) 

-- get  `divided AcCond` 
getAcCond :: Edgs -> (Edgs,[Ac],Edgs)  
getAcCond es = loop es ([],[],[]) where 
    loop []     ret       = ret 
    loop (e@(_,AcCond:as,_):es) (_,_,es') = case last as of 
            AcRecord i t   -> ([e], [AcRecord i t], es ++ es')
            _              -> loop es ([] , [],       e : es') 
    loop (e:es) (_,_,es') = loop es ([],[],e:es') 


--getAcChecks :: Node Int -> Node Int -> [Edgs] -> (Edgs, [Edgs]) 
getAcChecks i t rows = 
    let ([e], rows') = getAcCheck i t rows in 
    case last (arrow e) of 
        AcRecord i' t'  -> 
            let (es, rows'') = getAcChecks i' t' rows' in 
            (e:es, rows'') 
        _               ->  ([e], rows')  

        

getAcCheck :: Node Int -> Node Int -> [Edgs] -> (Edgs,[Edgs]) 
getAcCheck i t  rows = loop rows ([],[]) where 
    loop []       (e ,ret)  = (e,ret) 
    loop (es:ess) (cs,ret)  = loop ess (_c_ ++ cs, es':ret) where 
        (_c_,es') = partition (\e -> AcCheck i t ∈ arrow e) es 




branch :: Matrix (Edge Int (OR Action)) -> [Branch Action] 
branch = rev . branching . map removeEdgeOR . rows  



