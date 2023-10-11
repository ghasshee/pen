module Automata where 


import Set
import Prelude hiding ((>>)) 

-- || Ddfinition || 

data Node s = Q s                 
            | P (Node s,Node s) 
            | L [Node s]     deriving (Eq) 

instance Show s => Show (Node s) where 
    show (Q s) = "Q" ++ show s 
    show (P p) = show p 
    show (L l) = show l 
type Edge s a = (Node s,a,Node s) 
data Automata s a = A [Node s] [a] [Edge s a] (Node s) [Node s]     deriving (Eq,Show) 


-- || Validity   || 

valid :: (Eq s, Eq a) => Automata s a -> Bool 
valid (A qs as tr i t) = case tr of 
    []                                                          -> True
    (p,a,q):tr' | p `elem` qs && q `elem` qs && a `elem` as     -> valid (A qs as tr' i t) 
    _                                                           -> False


-- || Run        ||  

runAutomata :: (Ord a, Eq s) => Automata s a -> [a] -> Bool 
runAutomata (A qs as tr i t) w = run (A qs as tr i t) i w  where 
    run (A qs as tr init terminals) curr []         = curr `elem` terminals 
    run (A qs as tr init terminals) curr (c:w)      = case searchTransition curr c tr of 
        []              -> False 
        [(_,_,q)]       -> run (A qs as tr init terminals) q w 
        _               -> error "Non Deterministic Automata" 

searchTransition :: (Ord a, Eq s) => Node s -> a -> [Edge s a] -> [Edge s a] 
searchTransition curr a []                                      = [] 
searchTransition curr a ((p, a',q):trs) | a <= a' && curr == p  = (p,a',q) : searchTransition curr a trs  
searchTransition curr a (tr:trs)                                =            searchTransition curr a trs 
    

searchTransitionsFromNode :: (Ord a, Eq s) => Node s -> [Edge s a] -> [Edge s a]
searchTransitionsFromNode q trs = loop q [] trs where 
    loop q as []                                = as 
    loop q as ((p,a,p'):trs) | q==p             = (p,a,p') : loop q as trs 
    loop q as (tr:trs)                          = loop q as trs


thrd (_,_,a) = a 

nextNodes :: (Ord a, Eq s) => Node s -> [Edge s a] -> [Node s] 
nextNodes q trs = uniq $ map thrd $ searchTransitionsFromNode q trs 


mkSubsets :: (Ord a, Eq s) => [Edge s a] -> [(a,[Node s])]  
mkSubsets trs = loop trs [] where
    loop [] subsets             = subsets
    loop ((_,a,q):trs) subsets  = add (a,q) subsets where 
        add (a,q) []                        = [(a,[q])] 
        add (a,q) ((a',qs):ss) | a <= a'    = (a ,q>>qs):ss  
        add (a,q) ((a',qs):ss) | a >= a'    = (a',q>>qs):ss  
        add (a,q) ((a',qs):ss)              = add (a,q) ss


uniq :: Eq a => [a] -> [a] 
uniq [] = [] 
uniq (a:as) = if a `elem` as then uniq as else a : uniq as 

-- || Subset Construction || 

mkSubsetNode :: Eq s => [Node s] -> Node [s] 
mkSubsetNode [] = Q [] 
mkSubsetNode (Q i:qs) = Q (i:qs') where 
    Q qs' = mkSubsetNode qs

subsetConstruction :: (Ord a, Eq s) => Automata s a -> Automata [s] a
subsetConstruction (A qs as trs (Q i) t) = A nodes alphas trans init terminals
    where 
        loop q trs = 
            let aqs = searchTransitionsFromNode q trs in 
            let qqs = mkSubsets aqs in 
            let qss = map (\(a,qs) -> (a,mkSubsetNode qs)) qqs in 
            undefined 

        init  = Q [i]  
        terminals = undefined 
        nodes = undefined  
        alphas = undefined 
        trans = undefined 

-- || Trim || 

trim = undefined 
    



-- || Example    || 
--
--    'a'   
--    --
--   |  |
--   v /    'b'
--   (Q1) -------> ((Q2))
--
a1  :: Automata Int Char
a1  = A [Q 1,Q 2] ['a','b'] [(Q 1,'a',Q 1), (Q 1, 'b', Q 2)] (Q 1) [Q 2] 

eg0 = runAutomata a1 "b" 
eg1 = runAutomata a1 "ab" 
eg2 = runAutomata a1 "aab" 






data Terminal   = Trm
                | Non 
                deriving (Show)

type ALeaf s    = (Terminal, s) 

data ATree s a  = Init (ALeaf s) [(a, ATree s a)] 
                | Tr   (ALeaf s) [(a, ATree s a)] 
                | End  (ALeaf s) 
                deriving (Show)

eg :: ATree Int Char
eg = Init (Non,1) [('a', End (Non,1)), ('b', Tr (Trm,2) [('a', End (Trm,2))])]


atree2automata :: (Eq s, Ord a) => ATree s a -> Automata s a
atree2automata (Init i rest) = loop (Tr i rest) (A [] [] [] (Q (snd i)) []) where 
    loop (Tr node []) a = a 
    loop (Tr node ((a,atr):rest)) (A qs as trs i ts) = 
        let atmt = loop atr (A (q>>q'>>qs) (a>>as) ((q,a,q'):trs) i ts') in 
        loop2 node rest atmt where 
            q   = Q (snd node) 
            q'  = case atr of 
                    Init node' _ -> Q (snd node')
                    Tr   node' _ -> Q (snd node')
                    End  node'   -> Q (snd node') 
            ts' = case fst node of 
                    Trm -> q:ts
                    Non -> ts 
    loop (End node)   a = a 
    loop2 node [] a = a 
    loop2 node ((a,atr):atrs) (A qs as trs i ts) = 
        let atmt = loop2 node atrs (A (q'>>qs) (a>>as) ((q,a,q'):trs) i ts') in 
        loop atr atmt where 
            q  = Q (snd node) 
            q' = case atr of 
                    Init node' _ -> Q (snd node')
                    Tr   node' _ -> Q (snd node')
                    End  node'   -> Q (snd node') 
            ts' = case fst node of 
                    Trm -> q:ts
                    Non -> ts 
                

automata2atree :: (Eq s, Ord a) => Automata s a -> ATree s a
automata2atree (A qs as trs (Q i) ts) = 
    if Q i `elem` ts 
        then Init (Trm,i) (loop (Q i) trs ts) 
        else Init (Non,i) (loop (Q i) trs ts) where 
            loop (Q i) []  ts = []
            loop (Q i) trs ts = 
                let qaqs  = searchTransitionsFromNode (Q i) trs in 
                let aqs   = map (\(p,a,q) -> (a,q)) qaqs in 
                let node = if Q i `elem` ts then (Trm,i) else (Non,i) in 
                let trs' = setminus trs qaqs in  
                loop2 node aqs trs' [] 
            loop2 node []           trs l = l 
            loop2 node ((a,q):rest) trs l = 
                let Q i = q in 
                let node' = if Q i `elem` ts then (Trm,i) else (Non,i) in 
                loop2 node rest trs ((a,Tr node' (loop q trs ts)):l) 
                

