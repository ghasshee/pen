{-# LANGUAGE IncoherentInstances #-} 
module Automata where 


import Set
import Node
import Data.List (sort)
import Semiring 
import Prelude hiding ((>>)) 


-- || Definition || -- 

data    Automata s a = 
            A   [Node s] [a] [Edge s a] [Node s] [Node s]     
            --  Nodes  Alphabets Edges Initials Terminals
            deriving (Eq,Show) 




-- || Validity of Automata || --  

valid :: (Eq s, Eq a) => Automata s a -> Bool 
valid (A qs as tr i t) = case tr of 
    []                                                          -> True
    (p,a,q):tr' | p `elem` qs && q `elem` qs && a `elem` as     -> valid (A qs as tr' i t) 
    _                                                           -> False


-- || Run        ||  

runAutomata :: (Ord a, Eq s) => Automata s a -> [a] -> Bool 
runAutomata (A qs as tr i t) w = case i of 
    [init]  -> run (A qs as tr [init] t) init w  where     
        run (A qs as tr init terminals) curr []     = curr `elem` terminals 
        run (A qs as tr init terminals) curr (c:w)  = case searchTransition curr c tr of 
            []              -> False 
            [(_,_,q)]       -> run (A qs as tr init terminals) q w 
            _               -> error "Non Deterministic Automata" 
    _       -> error "Non Deterministics Automata"


searchTransition :: (Ord a, Eq s) => Node s -> a -> [Edge s a] -> [Edge s a] 
searchTransition curr a []                                      = [] 
searchTransition curr a ((p, a',q):trs) | a <= a' && curr == p  = (p,a',q) : searchTransition curr a trs  
searchTransition curr a (tr:trs)                                =            searchTransition curr a trs 
    

-- || Operations on automata || -- 


-- || Disjoint Union || -- 

new_node_other_than :: (Eq s, Num s) => [Node s] -> Node s  
new_node_other_than qs = loop qs 0 where 
    loop qs n = if Q n `elem` qs then loop qs (n+1) else Q n  

new_nodes :: (Eq s, Num s) => [Node s] -> [Node s] -> [Node s] 
new_nodes qs1 qs2 = loop qs1 qs2 [] where 
    loop qs1 []     qs2' = qs2' 
    loop qs1 (q:qs) qs2' = loop qs1 qs (new_node_other_than (qs1 ++ qs2') : qs2')  

new_node_mapping :: (Eq s, Num s) => [Node s] -> [Node s] -> [(Node s,Node s)] 
new_node_mapping qs1 qs2 = zip qs2 qs2' where 
    qs2' = new_nodes qs1 qs2

one_map_apply_edges :: (Eq s, Num s, Ord a) => (Node s, Node s) -> [Edge s a] -> [Edge s a] 
one_map_apply_edges (p,p') []            = [] 
one_map_apply_edges (p,p') ((q,a,q'):es) =
    if p == q 
        then if p == q' 
            then (p',a,p') : one_map_apply_edges (p,p') es
            else (p',a,q') : one_map_apply_edges (p,p') es
        else if p == q' 
            then (q ,a,p') : one_map_apply_edges (p,p') es 
            else (q ,a,q') : one_map_apply_edges (p,p') es 

map_apply_edges :: (Eq s, Num s, Ord a) => [(Node s,Node s)] -> [Edge s a] -> [Edge s a] 
map_apply_edges []       es = es 
map_apply_edges (m:maps) es = map_apply_edges maps (one_map_apply_edges m es)

one_map_apply_nodes (p,p') []       = []
one_map_apply_nodes (p,p') (q:qs)   = if p==q 
            then p': one_map_apply_nodes (p,p') qs
            else q : one_map_apply_nodes (p,p') qs 

map_apply_nodes []       qs = qs 
map_apply_nodes (m:maps) qs = map_apply_nodes maps (one_map_apply_nodes m qs) 

disjointUnionNodes :: (Num s, Eq s) => [Node s] -> [Node s] -> [Node s] 
disjointUnionNodes qs1 qs2 = qs1 ++ new_nodes qs1 qs2 

disjointUnionEdges :: (Num s,Eq s,Ord a) => 
    [Edge s a] -> [Edge s a] -> [Node s] -> [Node s] -> [Edge s a] 
disjointUnionEdges tr1 tr2 qs1 qs2 = 
    let map  = new_node_mapping qs1 qs2 in 
    let tr2' = map_apply_edges map tr2 in 
    tr1 ++ tr2' 
    

union :: (Eq s, Ord a, Num s) => Automata s a -> Automata s a -> Automata s a 
union (A qs1 as1 tr1 i1 t1) (A qs2 as2 tr2 i2 t2) = A qs as tr i t where 
    map = new_node_mapping qs1 qs2 
    qs  = disjointUnionNodes qs1 qs2
    as  = setplus as1 as2
    tr  = disjointUnionEdges tr1 tr2 qs1 qs2
    i   = setplus i1 (map_apply_nodes map i2)
    t   = setplus t1 (map_apply_nodes map t2) 



-- || subsets Node || -- 

subset2node :: [Node s] -> Node [s]
subset2node qs = loop qs [] where 
    loop []       node = Q node
    loop (Q i:qs) node = loop qs (node++ [i])  

mkSubsetNodes :: Ord s => [Node s] -> [Node [s]] 
mkSubsetNodes nodes = map subset2node $  map sort $ subsets nodes  


-- || Subset Construction || -- 

filter_transition a         = filter (\(q,a',q') -> a==a')  

filter_transition_domain q  = filter (\(p,a ,p') -> q == p ) 

transitionsfrom :: (Ord a, Eq s) => [Node s] -> a -> [Edge s a] -> [Edge s a]   
transitionsfrom qs a es = loop qs a es' where 
    es' = filter_transition a es
    loop []     a _  = [] 
    loop (q:qs) a es = filter_transition_domain q es ++ loop qs a es

alltransitionsfrom []     es = []
alltransitionsfrom (q:qs) es = filter_transition_domain q es ++ alltransitionsfrom qs es 

bindtrs :: (Ord s, Ord a) => [Edge s a] -> Maybe (Edge [s] a)
bindtrs trs = 
    let (qs,as,qs') = unzip3 trs in 
    case uniq as of 
    []  -> Nothing 
    [a] -> Just (subset2node (sort qs) , a, subset2node (sort qs'))  

rmMaybe [] = []
rmMaybe (Just a:xs) = a:rmMaybe xs
rmMaybe (Nothing:xs) = rmMaybe xs

subset_construction (A qs as trs is ts) = 
    let qss  = sort $ map sort $ subsets qs in 
    let qs'  = uniq $ map subset2node qss in 
    let trs' = uniq $ rmMaybe [ bindtrs (transitionsfrom q a trs) | a <- as, q <- qss ] in  
    let is'  = [subset2node $ sort is] in 
    let ts'  = uniq $ map subset2node $ map sort $ loop ts qss where 
        loop [] qss     = [] 
        loop (t:ts) qss = filter (t `elem`) qss ++ loop ts qss in 
    A qs' as trs' is' ts' 

-- || Trim || 




trim = undefined 

accessible is edges = loop is [] where 
    loop qs ret = 
        let es  = alltransitionsfrom qs edges in 
        let qs' = map third3 es in 
        let ret' = uniq $ es ++ ret in 
        if ret == ret' then (qs',ret) else loop qs' (es++ret) 

accessible' (A qs as trs is ts) = 
    let (qs',trs') = accessible is trs in 
    A qs' as trs' is (intersect ts qs')

third3 (_,_,c) = c


-- || Example    || 
--
--    'a'   
--    --
--   |  |
--   v /    'b'
--   (Q1) -------> ((Q2))
--
a1  :: Automata Int Char
a1  = A [Q 1,Q 2] ['a','b'] [(Q 1,'a',Q 1), (Q 1, 'b', Q 2)] [Q 1] [Q 2] 
a2  = A [Q 1,Q 2] ['a','b'] [(Q 1,'a',Q 1), (Q 1, 'a', Q 2), (Q 1, 'b', Q 2)] [Q 1] [Q 2] 

eg0 = runAutomata a1 "b" 
eg1 = runAutomata a1 "ab" 
eg2 = runAutomata a1 "aab" 




--------------------------------------
----          ATREE              -----
--------------------------------------

-- || Tree Representation of Automata || ----- 




data Terminal       = Trm
                    | Non 
                        deriving (Eq, Show)
(/\) Trm _ = Trm 
(/\) _ Trm = Trm
(/\) _   _ = Non

type ALeaf s        = (Terminal, s) 

data ABranch s a    = Br a (ATree s a) 
                        deriving (Eq, Show) 

data ATree s a      = Tr   (ALeaf s) [ABranch s a] 
                        deriving (Eq)

data AAtmt s a      = Init [ATree s a] 
                        deriving (Eq, Show) 

instance (Show s, Show a) => Show (ATree s a) where 
    show s = "\n" ++ showT "" s where 
      showT s (Tr (Trm,q) brs)  =   s ++ "+- ||Q" ++ show q ++ "||\n" 
                                    ++ showF(s++"    ")brs
      showT s (Tr (Non,q) brs)  =   s++ "+-- |Q" ++ show q ++ "| \n" 
                                    ++ showF(s++"    ")brs
      showF s []                =   ""
      showF s [Br a atr]        =   s ++ "+--" ++ show a ++ "\n" 
                                    ++ showT(s++ "    ")atr
      showF s (Br a atr:rest)   =   s ++ "+--" ++ show a ++ "\n" 
                                    ++ showT(s++ "|   ")atr
                                    ++ showF s rest 

{-- foldat :: (q -> br -> tr) -> (a -> tr -> br -> br) -> br -> br -> ATree s a -> tr --}
foldat g h d c (Tr q [])        = g q d 
foldat g h d c (Tr q xs)        = g q (foldaf g h d c xs) 
foldaf g h d c []               = c 
foldaf g h d c (Br a atr:xs)    = h a (foldat g h d c atr) (foldaf g h d c xs) 




-- || SUBSET CONSTRUCTION || -- 

data Switch = Go | Stop 


bandt (Tr (t,q) [] )       _ Stop  = Tr (t,[q]) []
bandt (Tr (t,q) [] )    atmt Go    = bandt (Tr (t,q) (get_1_step_from q atmt)) atmt Stop
bandt (Tr (t,q) brs)    atmt sw    = Tr (t,[q]) (bandf brs atmt sw) 

bandf []                _    _     = [] 
bandf (Br a tr : brs)  atmt sw     = br_a' : bandf brs' atmt sw
    where 
        (br_a', brs') = srch a tr brs (Br a (bandt tr atmt sw)) [] 

        srch a tr []             br ret           = (br , ret)
        srch a tr (Br a' tr':brs)br ret | a==a'   = srch a tr brs(bind br(Br a' tr'))ret 
        srch a tr (br'      :brs)br ret           = srch a tr brs br (br':ret)  

        bind (Br a(Tr(t,qs)brss))(Br _(Tr(t',q')brs)) 
            = Br a (Tr(t/\t',q':qs) (brss++bandf brs atmt sw)) 




nullify_tr (Tr s _) = Tr s []  
nullify_br (Br a t) = Br a (nullify_tr t) 

get_1_step_from node [] = [] 
get_1_step_from node (Br a (Tr (t,q) brs) : rest) | node == q = map nullify_br brs 
get_1_step_from node (Br a (Tr (t,q) brs) : rest) = 
    case get_1_step_from node brs of 
        []              -> get_1_step_from node rest
        brs             -> brs 

        
subsetconstruction_aatmt (Init trs) = 
    let brs = map (\tr -> Br '_' tr) trs in 
    case bandf brs brs Go of 
    [Br _ tr]           -> (Init [tr]) 


subsetconstruction a = (uniq_a . aatmt2automata . subsetconstruction_aatmt . automata2aatmt) a 

eg :: AAtmt Int Char
eg = Init [Tr (Non,1) [Br 'a' (Tr (Non,1)[]), Br 'a' (Tr (Trm,2) [Br 'a' (Tr (Trm,2)[])])]] 

eg_loop :: AAtmt Int Char
eg_loop = let loop = Tr (Non,1) [Br 'a' loop] in 
          Init [loop] 

cut_infinite_tree (Init trs) = 
    let brs = map (\tr -> Br '_' tr) trs in 
    loopf brs [] where 
        loopt (Tr s [] ) mem    = (Tr s [], mem)
        loopt (Tr s brs) mem    = if (Tr s brs) `elem` mem 
                                        then (Tr s [], mem) 
                                        else 
                                            if mem == mem' then (Tr s [],mem)  else 
                                                (Tr s (loopf brs mem'),mem') where 
                                            mem' = Tr s brs >> mem
        loopf []            mem = [] 
        loopf (Br a(Tr s b):brs) mem = if mem == (Tr s b >> mem)  
            then Br a (Tr s[]) : loopf brs mem 
            else Br a tr' : loopf brs mem' where 
                (tr', mem') = loopt (Tr s b) mem 
        

-- || " Automata <-> ATree Automata " conversion || -- 

searchTransitionsFromNode :: (Ord a, Eq s) => Node s -> [Edge s a] -> [Edge s a]
searchTransitionsFromNode q trs = loop q [] trs where 
    loop q as []                                = as 
    loop q as ((p,a,p'):trs) | q==p             = (p,a,p') : loop q as trs 
    loop q as (tr:trs)                          = loop q as trs

aatmt2automata :: (Eq s, Num s, Ord a) => AAtmt s a -> Automata s a 
aatmt2automata (Init []) = A [] [] [] [] []
aatmt2automata (Init (a:as)) = union (atree2automata a) (aatmt2automata (Init as))

automata2aatmt :: (Eq s, Ord a) => Automata s a -> AAtmt s a 
automata2aatmt (A _ _ _ [] _) = Init [] 
automata2aatmt (A qs as trs (i:is) ts) = Init (a':as') where 
    a'  = automata2atree (A qs as trs [i] ts) 
    Init as' = automata2aatmt (A qs as trs is ts) 


atree2automata :: (Eq s, Ord a) => ATree s a -> Automata s a
atree2automata (Tr i rest) = loop (Tr i rest) (A [] [] [] [Q (snd i)] []) where 
    loop (Tr (Trm,q) []) (A qs as trs is ts) = (A qs as trs is (Q q:ts)) 
    loop (Tr (Non,q) []) a = a 
    loop (Tr node (Br a atr:rest)) (A qs as trs i ts) = 
        let atmt = loop atr (A (q>>q'>>qs) (a>>as) ((q,a,q'):trs) i ts') in 
        loop2 node rest atmt where 
            q   = Q (snd node) 
            q'  = case atr of 
                    Tr   node' _ -> Q (snd node')
            ts' = case fst node of 
                    Trm -> q:ts
                    Non -> ts 
    loop2 node [] a = a 
    loop2 node (Br a atr:atrs) (A qs as trs i ts) = 
        let atmt = loop2 node atrs (A (q'>>qs) (a>>as) ((q,a,q'):trs) i ts') in 
        loop atr atmt where 
            q  = Q (snd node) 
            q' = case atr of 
                    Tr   node' _ -> Q (snd node')
            ts' = case fst node of 
                    Trm -> q:ts
                    Non -> ts 
                

automata2atree :: (Eq s, Ord a) => Automata s a -> ATree s a
automata2atree (A qs as trs [Q i] ts) = 
    if Q i `elem` ts 
        then Tr (Trm,i) (loop (Q i) trs ts) 
        else Tr (Non,i) (loop (Q i) trs ts) where 
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
                loop2 node rest trs (Br a (Tr node' (loop q trs ts)):l) 
                
uniq_a (A qs as trs is ts) = A (uniq qs) (uniq as) (uniq trs) (uniq is) (uniq ts)

instance (Num s) => Num [s] where 
    fromInteger i = [fromInteger i]   
    [i] + [j]     = [i+j] 
    _   +  _      = undefined 
    _   *  _      = undefined 
    abs _         = undefined 
    negate _      = undefined 
    signum _      = undefined 
    
