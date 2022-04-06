module RmVar where 

import Var
import Asm 
import Tree
import Let



rmSTACKTOPs :: UAST -> UAST
rmSTACKTOPs = fmap rm where 
    rm  (LET x (STACK n), us) = (LET x (VAR(nth' n us)), us) where 
        nth' n []     = Arg n 
        nth' 1 (u:us) = u 
        nth' n (u:us) = nth' (n-1) us
    rm  x                                                = x 

rmVARs  :: UAST -> UAST 
rmVARs tree = rmT tree tree where 
    rmT tree (RED(LET y(VAR x),_)_)  = RED (LET y x',us') (rmF tree ts') where RED (LET _ x', us') ts' = search tree x 
    rmT tree (RED a as)              = RED a (rmF tree as) 
    rmT tree (BLK b bs)              = BLK b (rmF tree bs)
    rmF tree []                      = [] 
    rmF tree (x:xs)                  = rmT tree x: rmF tree xs 


rmDUPs :: UAST -> UAST
rmDUPs = fmap rmDUP 
rmDUP (t,l) = rm (t,l) where
    rm (LET a DUP1, x:_)            = (LET a (VAR x),l)
    rm (LET a DUP1, _ )             = (LET a DUP1   ,l)
    rm (LET a DUP2, _:x:_)          = (LET a (VAR x),l)
    rm (LET a DUP2, _)              = (LET a DUP2   ,l)
    rm (LET a DUP3, _:_:x:_)        = (LET a (VAR x),l)
    rm (LET a DUP3, _)              = (LET a DUP3   ,l)
    rm (LET a DUP4, _:_:_:x:_)      = (LET a (VAR x),l)
    rm (LET a DUP4, _)              = (LET a DUP4   ,l)
    rm (LET a DUP5, _:_:_:_:x:_)    = (LET a (VAR x),l)
    rm (LET a DUP5, _)              = (LET a DUP5   ,l)
    rm x                            = x 




search :: UAST -> Var -> UAST  
search tree x = case searchT tree x of 
                    Just a  -> a 
                    Nothing -> BLK (UNIT $ UNDEFINED"",[]) []
    where
    searchT (RED(LET x o, us)as) a | a == x = Just $ RED (LET x o, us) as 
    searchT (RED(LET x o, us)as) a          = searchF as a 
    searchT (BLK(UNIT  o, us)as) a          = searchF as a 
    searchF []                   a          = Nothing 
    searchF (x:xs)               a          = case searchT x a of 
                                                    Nothing -> searchF xs a 
                                                    Just a  -> Just a                 
    

rmSWAPs :: UAST -> UAST 
rmSWAPs tree = rmSWAPT tree tree 
    where 
    rmSWAPT :: UAST -> UAST -> UAST 
    rmSWAPT (BLK b bs)                      tree  = rmSWAPF (bs) tree                    
    rmSWAPT (RED a as)                      tree  = rmSWAPF (as) tree 
    rmSWAPF :: [UAST] -> UAST -> UAST 
    rmSWAPF []                              tree  = tree  
    rmSWAPF (BLK(UNIT SWAP1,a:b:_)_:xs)     tree  = rmSWAPF xs (swap tree a b tree) 
    rmSWAPF (BLK(UNIT SWAP2,a:_:b:_)_:xs)   tree  = rmSWAPF xs (swap tree a b tree) 
    rmSWAPF (BLK(UNIT SWAP3,a:_:_:b:_)_:xs) tree  = rmSWAPF xs (swap tree a b tree) 
    rmSWAPF (x:xs)                          tree  = rmSWAPF xs (rmSWAPT x tree)

swap :: UAST -> Var -> Var -> UAST -> UAST 
swap = swapT 
    where 
    swapT (RED(LET x a',us)ts)           a b tr | x==a        = (search tr b) 
    swapT (RED(LET x b',us)ts)           a b tr | x==b        = (search tr a) 
    swapT (RED t ts)                     a b tr               = RED t             (swapF ts a b tr) 
    swapT (BLK t ts)                     a b tr               = BLK t             (swapF ts a b tr) 
    swapF (BLK(UNIT SWAP1,x:y:_    )_:xs)a b tr | a==x&&b==y  =                    swapF xs a b tr
    swapF (BLK(UNIT SWAP2,x:_:y:_  )_:xs)a b tr | a==x&&b==y  =                    swapF xs a b tr
    swapF (BLK(UNIT SWAP3,x:_:_:y:_)_:xs)a b tr | a==x&&b==y  =                    swapF xs a b tr
    swapF (x:xs) a b tr                                       = swapT x a b tr   : swapF xs a b tr
    swapF []     a b tr                                       = [] 


