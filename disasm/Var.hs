module Var where 

import Asm 
import Tree 


data Var    = X Integer 

instance Show Var where 
    show (X n) = "X" ++ show n

data Term   = LET Var OPCODE
            | UNIT OPCODE 

instance Show Term where 
    show (LET v o) = "let " ++ show v ++ " = " ++ show o 
    show (UNIT o)  = show o 

letbind o n = LET (X n) o 

var t = fst $ vt t 0 where 
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


elder_unclesLet = flip walkt [] where
    walkt (RED  a []) uncles        = RED  (a,uncles) [] 
    walkt (BLK  a []) uncles        = BLK  (a,uncles) [] 
    walkt (RED  a xs) uncles        = RED  (a,uncles) (walkf xs uncles) 
    walkt (BLK  a xs) uncles        = BLK  (a,uncles) (walkf xs uncles) 
    walkf []          uncles        = []
    walkf (x:xs)      uncles        = walkt x uncles : case x of 
                                            RED (LET v _) _  -> walkf xs (v:uncles) 
                                            _                -> walkf xs uncles 
    
