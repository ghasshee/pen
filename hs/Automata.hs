-- || DEFINITION

data Node s = Q s                 deriving (Eq)
type Edge s a = (Node s,a,Node s) 
data Automata s a = A [Node s] [a] [Edge s a] (Node s) [Node s]     deriving (Eq) 


-- || VALIDITY of Automata || 

valid :: (Eq s, Eq a) => Automata s a -> Bool 
valid (A qs as tr i t) = case tr of 
    []                                                          -> True
    (p,a,q):tr' | p `elem` qs && q `elem` qs && a `elem` as     -> valid (A qs as tr' i t) 
    _                                                           -> False


-- || RUN ||  

runAutomata :: (Eq a, Eq s) => Automata s a -> [a] -> Bool 
runAutomata (A qs as tr i t) w = run (A qs as tr i t) i w  where 
    run (A qs as tr init terminals) curr []         = curr `elem` terminals 
    run (A qs as tr init terminals) curr (c:w)      = case searchTransition curr c tr of 
        []              -> False 
        [(_,_,q)]       -> run (A qs as tr init terminals) q w 
        _               -> error "Non Deterministic Automata" 

searchTransition :: (Eq a, Eq s) => Node s -> a -> [Edge s a] -> [Edge s a] 
searchTransition curr a []                                      = [] 
searchTransition curr a ((p, a',q):trs) | a == a' && curr == p  = (p,a',q) : searchTransition curr a trs  
searchTransition curr a (tr:trs)                                =            searchTransition curr a trs 
    

-- || Example || 
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




