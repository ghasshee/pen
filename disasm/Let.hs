module Let where 

import Var 
import Asm 
import Tree 
import Test

data Let a'     = LET Var a'
                | UNIT a' 
                deriving Eq

instance Show a' => Show (Let a') where 
    show (LET v o) = show v ++ " = " ++ show o 
    show (UNIT o)  = show o 



var :: RBTree OPCODE -> Integer -> (RBTree (Let OPCODE), Integer) 
var t = vt t where 
    vt (RED o []) n     = (RED (LET(X n)o) [],n+1)  
    vt (RED o xs) n     = (RED (LET(X n)o) xs', m) where
                                (xs', m)    = vf xs (n+1) 
    vt (BLK o []) n     = (BLK (UNIT o) [],n) 
    vt (BLK o xs) n     = (BLK (UNIT o) xs'   , m) where 
                                (xs', m)    = vf xs n 
    vf []         n     = ([],n) 
    vf (x:xs)     n     = (x' : xs', k) where
                                (x',m)      = vt x n 
                                (xs',k)     = vf xs m

mapvar :: [RBTree OPCODE] -> [RBTree (Let OPCODE)]   
mapvar ts = mvar ts 0 where 
    mvar []     n       = []  
    mvar (t:ts) n       = t' : mvar ts m where 
                                (t', m)     = var t n



uncleVars :: RBTree (Let OPCODE) -> RBTree (Let OPCODE,[Var]) 
uncleVars = flip walkt [] where
    walkt (RED  a [])     uncles    = RED  (a,uncles) [] 
    walkt (BLK  a [])     uncles    = BLK  (a,uncles) [] 
    walkt (RED  a xs)     uncles    = RED  (a,uncles)     (walkf xs uncles 0) 
    walkt (BLK(UNIT JUMPI)xs)uncles = BLK  (UNIT JUMPI,uncles) (walkf xs uncles 2) 
    walkt (BLK(UNIT JUMP) xs)uncles = BLK  (UNIT JUMP,uncles) (walkf xs uncles 1) 
    walkt (BLK  a xs)     uncles    = BLK  (a,uncles)     (walkf xs uncles 0) 
    walkf []              uncles  n = []
    walkf (BLK(UNIT SEQ)os:xs)uncles n = walkt (BLK (UNIT SEQ) os) (drop n uncles) : walkf xs uncles n
    walkf (x:xs)          uncles  n = walkt x uncles : case x of 
                                            RED (LET _ (STACKTOP _))_   -> walkf xs uncles     n  
                                            RED (LET v _) _             -> walkf xs (v:uncles) n 
                                            _                           -> walkf xs uncles     n
    




rmSTACKTOPs :: AST -> AST
rmSTACKTOPs = fmap rmSTACKTOP  
rmSTACKTOP (t,l) = rm (t,l) where 
    rm  (LET x (STACKTOP n), uncles) | length uncles > n = (LET x (VAR(uncles !! (n-1))), l) 
    rm  x                                                = x 


rmDUPs :: RBTree (Let OPCODE, [Var]) -> RBTree (Let OPCODE, [Var]) 
rmDUPs = fmap rmDUP  
rmDUP (t,l) = rm (t,l) where 
    rm (LET a DUP1, x:xs)           = (LET a (VAR x),l)
    rm (LET x DUP1, _ )             = (LET x DUP1,l)
    rm (LET a DUP2, _:x:xs)         = (LET a (VAR x),l)
    rm (LET x DUP2, _)              = (LET x DUP2,l)
    rm (LET a DUP3, _:_:x:xs)       = (LET a (VAR x),l)
    rm (LET x DUP3, _)              = (LET x DUP3 ,l)
    rm (LET a DUP4, _:_:_:x:xs)     = (LET a (VAR x),l)
    rm (LET x DUP4, _)              = (LET x DUP4 ,l)
    rm (LET a DUP5, _:_:_:_:x:xs)   = (LET a (VAR x),l)
    rm (LET x DUP5, _)              = (LET x DUP5 ,l)
    rm x                            = x 



type AST = RBTree (Let OPCODE, [Var]) 

search :: AST -> Var -> AST  
search tree x = case searchT tree x of 
                    Just a  -> a 
                    Nothing -> error "Variable Not found" 

searchT (RED(LET x o, us)as) a | a == x = Just $ RED (LET x o, us) as 
searchT (RED(LET x o, us)as) a          = searchF as a 
searchT (BLK(UNIT  o, us)as) a          = searchF as a 
searchF []                   a          = Nothing 
searchF (x:xs)               a          = case searchT x a of 
                                                Nothing -> searchF xs a 
                                                Just a  -> Just a                 


rmSWAPs :: AST -> AST 
rmSWAPs tree = rmSWAPT tree tree 

rmSWAPT :: AST -> AST -> AST 
rmSWAPT (BLK b bs)                  tree  = rmSWAPF (bs) tree                    
rmSWAPT (RED a as)                  tree  = rmSWAPF (as) tree 
rmSWAPF :: [AST] -> AST -> AST 
rmSWAPF []                          tree  = tree  
rmSWAPF (BLK(UNIT SWAP1,a:b:_)_:xs) tree  = rmSWAPF xs (swap tree a b tree) 
rmSWAPF (x:xs)                      tree  = rmSWAPF xs (rmSWAPT x tree)

swap :: AST -> Var -> Var -> AST -> AST 
swap = swapT 
swapT (RED(LET x a',us)ts)       a b tree | x == a  = (search tree b) 
swapT (RED(LET x b',us)ts)       a b tree | x == b  = (search tree a) 
swapT (RED t ts)                 a b tree           = RED t                           (swapF ts a b tree) 
swapT (BLK t ts)                 a b tree           = BLK t                           (swapF ts a b tree) 
swapF (BLK(UNIT SWAP1,x:y:_)_:xs)a b tree | a == x && b == y =           swapF xs a b tree
swapF (x:xs) a b tree                               = swapT x a b tree : swapF xs a b tree
swapF []     a b tree                               = [] 





