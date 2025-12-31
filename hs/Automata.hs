{-# LANGUAGE IncoherentInstances #-} 
module Automata 
( Automata (A) 
, renodeAutomata 
, runAutomata 
, matchAutomata
, trim
, accessible 
, coaccessible
, union , (∪) 
, intersect , (∩) 
, uniq 
, subset_construction ) 
where 


import Logic 
import Set
import Any 
import Mapping 
import Node
import Edge
import Data.List (sort)
import Semiring 
import Prelude hiding (any, (>>)) 

----------------------------------
-----         AUTOMATA       -----
----------------------------------


noDFA :: forall a. a
noDFA = error "Non Deterministic Automata"



-- || Definition || -- 

data    Automata node alpha = 
            A   [node] [alpha] [Edge node alpha] [node] [node]     
            --  Nodes  Alphabets Edges Initials Terminals
            deriving (Eq,Show) 





-- || Embedding Alphabets of Automata into Any type || -- 

embed_any :: Automata node a -> Automata node (Any a) 
embed_any (A qs as es is ts) = A qs as' es' is ts where 
    as' = pure <$> as
    es' = (\(q,a,p) -> (q, pure a, p)) <$> es 




-- || Renumbering the Nodes || --

renodeAutomata (A qs as es is ts) = A qs' as es' is' ts' 
    where 
        table   = renode_table qs 
        qs'     = renodes     table qs
        is'     = renodes     table is
        ts'     = renodes     table ts 
        es'     = renodeEdges table es



-- || Validity of Automata || --  

valid :: (Eq node, Eq a) => Automata node a -> Bool 
valid (A qs as es is ts) =  qs' <== qs && as' <== as && ps' <== qs   where 
    (qs', as', ps')     = unzip3 es  
    l' <== l            = foldr (&&) True $ (∈ l) <$> l'    



-- || Run        ||  

runAutomata :: (Ord a, Eq node) => Automata node a -> [a] -> Bool 
runAutomata atmt w = matchAutomata (embed_any atmt) w  
 

-- || Match || -- 

matchAutomata :: (Ord a, Eq node) => Automata node (Any a) -> [a] -> Bool 
matchAutomata (A _ _ es is ts) w = case is of 
    [i]  -> run i w  where     
        run curr []     = curr ∈ ts 
        run curr (c:w)  = case nextMatch  curr c es of 
            []  -> False 
            [e] -> run (cod e) w 
            _   -> noDFA
    _       -> noDFA 

nextMatch :: (Eq a, Eq node) => node -> a -> [Edge node (Any a)] -> [Edge node (Any a)]
nextMatch curr a []                                 = [] 
nextMatch curr a ((p,b,q):es)   | a ~< b && curr==p = (p,b,q) : nextMatch curr a es 
                                | otherwise         =           nextMatch curr a es 




-- || Operations on automata || -- 

instance (Ord a, Ord s, Num s, Enum s) => ListOperations (Automata (Node s))  a  where 
    toList      = undefined 
    fromList    = undefined 
    (∈)         = undefined 
    (~=~)       = undefined 
    (+++)       = undefined 
    (>>)        = undefined 
    uniq (A qs as es is ts)     = A (uniq qs)(uniq as)(uniq es)(uniq is)(uniq ts) 

instance (Ord a, Ord s, Num s, Enum s) => SetOperations (Automata (Node s)  a) where 
    φ           = A [] [] [] [] [] 
    A qs1 as1 es1 is1 ts1 ∪ A qs2 as2 es2 is2 ts2   = A qs as es is ts where 
        table   = renode_table qs' 
        qs'     = qs1 +++ qs2
        qs      = renodes table qs' 
        as      = as1 ∪ as2
        es      = renodeEdges table (leftEdges es1 ++ rightEdges es2)
        is      = renodes table (is1 +++ is2)  
        ts      = renodes table (ts1 +++ ts2) 
    A qs1 as1 es1 is1 ts1 ∩ A qs2 as2 es2 is2 ts2   = A qs as es is ts where 
        table   = renode_table qs' 
        qs'     = [(q1,q2) | q1 <- qs1, q2 <- qs2]
        qs      = renodes table qs' 
        as      = as1 ∩ as2 
        es      = renodeEdges table $ intersect_edges [(e1,e2) | e1 <- es1, e2 <- es2]
        is      = renodes table [(i1,i2) | i1 <- is1, i2 <- is2] 
        ts      = renodes table [(t1,t2) | t1 <- ts1, t2 <- ts2] 
    (\\)                        = undefined 
    subsets                     = undefined 






-- || Subset Construction || -- 

filter_edge_arr  a     = filter ((==a) . arrow)  
filter_edge_dom  q     = filter ((==q) . dom) 
filter_edge_cod  q     = filter ((==q) . cod) 
filter_edge_doms qs es = concat $ (\q -> filter_edge_dom q es) <$> qs   
filter_edge_cods qs es = concat $ (\q -> filter_edge_cod q es) <$> qs   

edgesfrom :: (Ord node, Eq a) => [node] -> a -> [Edge node a] -> Edge [node] a 
edgesfrom qs a es = (qs, a, ps) where 
    ps = sort $ uniq $ cod <$> (filter  ( (∈ qs) . dom )  $ filter_edge_arr a es)

alphabets  :: Ord a => [Any a] -> [a] 
alphabets  as = loop as [] where 
    loop []         alphas = alphas 
    loop (Ex cs:as) alphas = loop as (cs ∪ alphas) 
    loop (In cs:as) alphas = loop as (cs ∪ alphas) 

separate_edge alphabets (q,In cs,p) =                        [(q, In[c], p) | c <- cs]  
separate_edge alphabets (q,Ex as,p) = (q, Ex alphabets, p) : [(q, In[c], p) | c <- cs] where 
    In cs  = Ex as \\ Ex alphabets 

separate_edges :: Ord a => [Any a] -> [Edge node(Any a)] -> ([Edge node(Any a)], [Any a])  
separate_edges as es = (es',as') where 
    es' = concat $ separate_edge (alphabets as) <$> es 
    as' = Ex (alphabets as) : (In <$> (pure <$> (alphabets as))) 


subset_construction :: (Ord node, Ord a) => Automata node (Any a) -> Automata [node] (Any a) 
subset_construction (A qs _ es is ts) = A qss as' es'' is' ts' where 
    qss         = sort <$> subsets qs 
    as          = uniq $ arrow <$> es
    (es',as')   = separate_edges as es 
    es''        = [ edgesfrom qs a es' | a <- as', qs <- qss ]  
    is'         = [sort is] 
    ts'         = uniq $ sort <$> loop ts where 
        loop []     = []
        loop (t:ts) = filter (t ∈) qss ++ loop ts 


-- || Trim || -- 

trim (A qs as es is ts) = A (qs1 ∩ qs2) as (es1 ∩ es2) is' ts' where     
    A qs1 _ es1 _ ts' = accessible  (A qs as es is ts) 
    A qs2 _ es2 is' _ = coaccessible (A qs as es is ts) 

accessible :: (Eq node, Eq a, Ord a, Ord node) => Automata node a -> Automata node a 
accessible (A qs as es is ts) = loop is is [] where 
    loop ps _qs _es | _es ~=~ _es'  = A _qs' as _es' is (ts ∩ _qs') 
                    | otherwise     = loop qs' _qs' _es' where 
        es'     = filter_edge_doms ps es 
        qs'     = cod <$> es'
        _qs'    = _qs ∪ qs' 
        _es'    = _es ∪ es'
        
coaccessible :: (Eq node, Eq a, Ord a, Ord node) => Automata node a -> Automata node a 
coaccessible (A qs as es is ts) = loop ts ts [] where 
    loop qs _ps _es | _es ~=~ _es'  = A _ps' as _es' (is ∩ _ps')  ts   
                    | otherwise     = loop ps _ps' _es'     where 
        es'     = filter_edge_cods qs es
        ps      = dom <$> es' 
        _ps'    = _ps ∪ ps 
        _es'    = _es ∪ es 





-- || Example    || 
--
--    'a'   
--    --
--   |  |
--   v /    'b'
--   (Q1) -------> ((Q2))
--
a1  :: Automata(Node Int)Char
a1  = A [Q 1,Q 2] ['a','b'] [(Q 1,'a',Q 1), (Q 1, 'b', Q 2)] [Q 1] [Q 2] 
a2  = A [Q 1,Q 2] ['a','b'] [(Q 1,'a',Q 1), (Q 1, 'a', Q 2), (Q 1, 'b', Q 2)] [Q 1] [Q 2] 

eg0 = runAutomata a1 "b" 
eg1 = runAutomata a1 "ab" 
eg2 = runAutomata a1 "aab" 


