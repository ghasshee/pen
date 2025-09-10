module ATree {-# WARNING "Automata.hs / ATree.hs unused modules" #-} where 

import Logic 
import Set 
import Mapping 
import Node 
import Edge 
import Automata 

import Prelude hiding ((>>)) 


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

renode_table_AAtmt (Init atrs)  = zip nodes [0..] where 
    nodes = concat (foldat (\(_,q) d -> q:d) (\a tr brs -> tr ++ brs) [] [] <$> atrs) 

renodeAAtmt (Init atrs)   = Init (foldat (\(trm, q) d  -> Tr (trm, mapping table q) d) (\a tr brs -> Br a tr : brs)  [] [] <$> atrs) where 
    table = renode_table_AAtmt (Init atrs) 
    


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
    let brs = map (\tr -> Br mempty tr) trs in 
    case bandf brs brs Go of 
    [Br _ tr]           -> (Init [tr]) 


subsetconstruction a = (uniq . aatmt2automata . renodeAAtmt . subsetconstruction_aatmt . automata2aatmt) a 

eg :: AAtmt Int Char
eg = Init [Tr (Non,1) [Br 'a' (Tr (Non,1)[]), Br 'a' (Tr (Trm,2) [Br 'a' (Tr (Trm,2)[])])]] 

eg_loop :: AAtmt Int Char
eg_loop = let loop = Tr (Non,1) [Br 'a' loop] in 
          Init [loop] 

cut_infinite_tree (Init trs) = 
    let brs = map (\tr -> Br '_' tr) trs in 
    loopf brs [] where 
        loopt (Tr s [] ) mem    = (Tr s [], mem)
        loopt (Tr s brs) mem    = if (Tr s brs) ∈ mem 
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

searchTransitionsFromNode :: (Ord a, Eq s) => Node s -> [Edge(Node s)a] -> [Edge(Node s)a]
searchTransitionsFromNode q trs = loop q [] trs where 
    loop q as []                                = as 
    loop q as ((p,a,p'):trs) | q==p             = (p,a,p') : loop q as trs 
    loop q as (tr:trs)                          = loop q as trs

aatmt2automata :: (Ord a, Num s, Enum s, Ord s) => AAtmt s a -> Automata(Node s)a 
aatmt2automata (Init [])     = A [] [] [] [] []
aatmt2automata (Init (a:as)) = union (atree2automata a) (aatmt2automata (Init as))

automata2aatmt :: (Eq s, Ord s, Ord a) => Automata(Node s)a -> AAtmt s a 
automata2aatmt (A _ _ _ [] _) = Init [] 
automata2aatmt (A qs as trs (i:is) ts) = Init (a':as') where 
    a'  = automata2atree (A qs as trs [i] ts) 
    Init as' = automata2aatmt (A qs as trs is ts) 


atree2automata :: (Eq s, Ord s, Ord a) => ATree s a -> Automata(Node s)a
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
                

automata2atree :: (Eq s, Ord s, Ord a) => Automata(Node s)a -> ATree s a
automata2atree (A qs as trs [Q i] ts) = 
    if Q i ∈ ts 
        then Tr (Trm,i) (loop (Q i) trs ts) 
        else Tr (Non,i) (loop (Q i) trs ts) where 
            loop (Q i) []  ts = []
            loop (Q i) trs ts = 
                let qaqs  = searchTransitionsFromNode (Q i) trs in 
                let aqs   = map (\(p,a,q) -> (a,q)) qaqs in 
                let node = if Q i ∈ ts then (Trm,i) else (Non,i) in 
                let trs' = trs \\ qaqs in  
                loop2 node aqs trs' [] 
            loop2 node []           trs l = l 
            loop2 node ((a,q):rest) trs l = 
                let Q i = q in 
                let node' = if Q i ∈ ts then (Trm,i) else (Non,i) in 
                loop2 node rest trs (Br a (Tr node' (loop q trs ts)):l) 
                

instance (Num s) => Num [s] where 
    fromInteger i = [fromInteger i]   
    [i] + [j]     = [i+j] 
    _   +  _      = undefined 
    _   *  _      = undefined 
    abs _         = undefined 
    negate _      = undefined 
    signum _      = undefined 
    
