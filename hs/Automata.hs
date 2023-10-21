module Automata where 


import Set
import Semiring 
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
data Automata s a = A [Node s] [a] [Edge s a] [Node s] [Node s]     deriving (Eq,Show) 



-- || Validity   || 

valid :: (Eq s, Eq a) => Automata s a -> Bool 
valid (A qs as tr i t) = case tr of 
    []                                                          -> True
    (p,a,q):tr' | p `elem` qs && q `elem` qs && a `elem` as     -> valid (A qs as tr' i t) 
    _                                                           -> False


-- || Run        ||  

runAutomata :: (Ord a, Eq s) => Automata s a -> [a] -> Bool 
runAutomata (A qs as tr i t) w = case i of 
    _       -> error "Non Deterministric Automata" 
    [init]  -> run (A qs as tr [init] t) init w  where     
        run (A qs as tr init terminals) curr []     = curr `elem` terminals 
        run (A qs as tr init terminals) curr (c:w)  = case searchTransition curr c tr of 
            []              -> False 
            [(_,_,q)]       -> run (A qs as tr init terminals) q w 
            _               -> error "Non Deterministic Automata" 


searchTransition :: (Ord a, Eq s) => Node s -> a -> [Edge s a] -> [Edge s a] 
searchTransition curr a []                                      = [] 
searchTransition curr a ((p, a',q):trs) | a <= a' && curr == p  = (p,a',q) : searchTransition curr a trs  
searchTransition curr a (tr:trs)                                =            searchTransition curr a trs 
    

-- || Operations on automata || -- 

union :: (Eq s, Ord a) => Automata s a -> Automata s a -> Automata s a 
union (A qs1 as1 tr1 i1 t1) (A qs2 as2 tr2 i2 t2) = A qs as tr i t where 
    qs = setplus qs1 qs2
    as = setplus as1 as2
    tr = setplus tr1 tr2
    i  = setplus i1 i2 
    t  = setplus t1 t2 




-- || || 

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

-- || Subset Construction || 

mkSubsetNode :: Eq s => [Node s] -> Node [s] 
mkSubsetNode [] = Q [] 
mkSubsetNode (Q i:qs) = Q (i:qs') where 
    Q qs' = mkSubsetNode qs

subsetConstruction :: (Ord a, Eq s) => Automata s a -> Automata [s] a
subsetConstruction (A qs as trs is ts) = undefined 
    where 
        loop q trs = 
            let aqs = searchTransitionsFromNode q trs in 
            let aqss = mkSubsets aqs in 
            let aQs = map (\(a,qs) -> (a,mkSubsetNode qs)) aqss in 
            undefined 
        i = undefined 
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
a1  = A [Q 1,Q 2] ['a','b'] [(Q 1,'a',Q 1), (Q 1, 'b', Q 2)] [Q 1] [Q 2] 
a2  = A [Q 1,Q 2] ['a','b'] [(Q 1,'a',Q 1), (Q 1, 'a', Q 2), (Q 1, 'b', Q 2)] [Q 1] [Q 2] 

eg0 = runAutomata a1 "b" 
eg1 = runAutomata a1 "ab" 
eg2 = runAutomata a1 "aab" 






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
    show s = "\n" ++ showT "    " s where 
      showT s (Tr (Trm,q) brs)  = s++"+- ||Q" ++ show q ++ "||\n" ++ showF("    "++s)brs
      showT s (Tr (Non,q) brs)  = s++"+-- |Q" ++ show q ++ "| \n" ++ showF("    "++s)brs
      showF s []                = ""
      showF s [Br a atr]        = s++ "+--" ++ show a ++ "\n" ++ showT(s++ "   ")atr
      showF s (Br a atr:rest)   = s++ "+--" ++ show a ++ "\n" ++ showT(s++ "|  ")atr
                                        ++ showF s rest 

{-- foldat :: (q -> br -> tr) -> (a -> tr -> br -> br) -> br -> br -> ATree s a -> tr --}
foldat g h d c (Tr q [])        = g q d 
foldat g h d c (Tr q xs)        = g q (foldaf g h d c xs) 
foldaf g h d c []               = c 
foldaf g h d c (Br a atr:xs)    = h a (foldat g h d c atr) (foldaf g h d c xs) 


bandt (Tr (t,q) [] )           _ False      = Tr (t,[q]) []
bandt (Tr (t,q) [] )        atmt True       = 
    bandt (Tr (t,q) (get_1_step_from q atmt)) atmt False
bandt (Tr (t,q) brs)          atmt tm       = Tr (t,[q]) (bandf brs atmt tm) 
bandf []                      atmt tm       = [] 
bandf (Br a tr : rest)        atmt tm       = br_a' : bandf rest' atmt tm where 
    (br_a', rest') = srch a tr rest (Br a (bandt tr atmt tm)) [] 
    srch a tr []             br ret           = (br , ret)
    srch a tr (Br a' tr':brs)br ret | a==a'   = srch a tr brs(bind br(Br a' tr'))ret 
    srch a tr (r:brs)        br ret           = srch a tr brs br (r:ret)  
    bind(Br a(Tr(t,qs)brss))(Br _(Tr(t',q')brs)) 
        = Br a (Tr(t/\t',q':qs) (brss++bandf brs atmt tm)) 


get_1_step_from node [] = [] 
get_1_step_from node (Br a (Tr (t,q) brs) : rest) | node == q =
    map (\(Br a (Tr s _)) -> Br a (Tr s [])) brs 
get_1_step_from node (Br a (Tr (t,q) brs) : rest) = 
    case get_1_step_from node brs of 
        []              -> get_1_step_from node rest
        brs             -> brs 

        
subsetconstruction_aatmt (Init trs) = 
    let brs = map (\tr -> Br '_' tr) trs in 
    case bandf brs brs True of 
    [Br _ tr]           -> (Init [tr]) 


subsetconstruction a = (uniq_a . aatmt2automata . subsetconstruction_aatmt . automata2aatmt) a 

eg :: AAtmt Int Char
eg = Init [Tr (Non,1) [Br 'a' (Tr (Non,1)[]), Br 'a' (Tr (Trm,2) [Br 'a' (Tr (Trm,2)[])])]] 

eg_loop :: AAtmt Int String
eg_loop = let loop = Tr (Non,1) [Br "a" loop] in 
          Init [loop] 


aatmt2automata :: (Eq s, Ord a) => AAtmt s a -> Automata s a 
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
