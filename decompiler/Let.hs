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

type AST    = RBTree OPCODE 
type LAST   = RBTree (Let OPCODE)
type UAST   = RBTree (Let OPCODE, [Var]) 



-- | Add Let Variables to AST 
mapvar :: [AST] -> [LAST]   
mapvar ts = mvar ts 0 where 
    mvar []     n       = []  
    mvar (t:ts) n       = t' : mvar ts m where 
                                (t', m)     = var (t,n)

local t = fst $ var (t,0) 

var :: (AST, Integer) -> (LAST, Integer) 
var = vt where 
    vt (RED o [], n)    = (RED (LET(X n)o) [] , n+1 )  
    vt (RED o xs, n)    = (RED (LET(X n)o) xs', m   ) where
                                (xs', m)    = vf (xs, n+1) 
    vt (BLK o [], n)    = (BLK (UNIT    o) [] , n) 
    vt (BLK o xs, n)    = (BLK (UNIT o) xs'   , m) where 
                                (xs', m)    = vf (xs, n) 
    vf ([]      , n)    = ([]      , n) 
    vf (x:xs    , n)    = (x' : xs', k) where
                                (x',m)      = vt (x, n) 
                                (xs',k)     = vf (xs, m)


-- | Add Uncle Variable Lists i.e. Stack Values
uncleVars :: LAST -> UAST
uncleVars = flip walkt (fmap Arg [1..16]) where
    walkt (RED  a [])     uncles    = RED  (a,         uncles) [] 
    walkt (BLK  a [])     uncles    = BLK  (a,         uncles) [] 
    walkt (RED  a xs)     uncles    = RED  (a,         uncles) (walkf xs uncles 0) 
    walkt (BLK(UNIT JUMPI)xs)uncles = BLK  (UNIT JUMPI,uncles) (walkf xs uncles 2) 
    walkt (BLK(UNIT JUMP) xs)uncles = BLK  (UNIT JUMP, uncles) (walkf xs uncles 1) 
    walkt (BLK  a xs)     uncles    = BLK  (a,         uncles) (walkf xs uncles 0) 
    walkf []              uncles  n = []
    walkf (BLK(UNIT SEQ)os:xs)uncles n = walkt (BLK (UNIT SEQ) os) (drop n uncles) : walkf xs uncles n
    walkf (x:xs)          uncles  n = walkt x uncles : case x of 
                                            RED (LET _ (STACK _))_   -> walkf xs uncles     n  
                                            RED (LET v _) _             -> walkf xs (v:uncles) n 
                                            _                           -> walkf xs uncles     n

-- | Remove Uncle Lists     
unUncle :: UAST -> AST 
unUncle = walkT where 
    walkT (RED(LET v a,us)as)   = RED a (walkF as)
    walkT (BLK(UNIT  b,us)bs)   = BLK b (walkF bs)
    walkF []                    = []
    walkF (x:xs)                = walkT x : walkF xs 







