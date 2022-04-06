module Hoist where 

import Var
import Asm 
import Tree
import Let
import Data.List ((\\)) 



rmPOPs   :: UAST -> UAST 
rmPOPs tree = fst $ rmT tree [] where 
    rmT (RED(LET x o, us)ts) rm = (RED (LET x o,us \\ rm) ts', rm') where (ts', rm') = rmF ts rm  
    rmT (BLK(UNIT  o, us)ts) rm = (BLK (UNIT  o,us \\ rm) ts', rm') where (ts', rm') = rmF ts rm 
    rmF (BLK(UNIT POP,us)[RED(LET x (STACK 1),Arg k:as)[]]:ts) rm = rmF ts ([Arg k] ++ rm)   
    rmF [] rm                   = ([], rm) 
    rmF (t:ts)               rm = (t':ts', rm'')
        where 
            (t',rm') = rmT t rm   
            (ts',rm'') = rmF ts rm'





rmDUPs :: UAST -> UAST
rmDUPs = fmap rmDUP 
rmDUP (t,l) = rm (t,l) where
    rm (LET a DUP1, x:_)            = (LET a (VAR x),l)
    rm (LET a DUP1, _)              = (LET a (VAR $ Arg 1),l)
    rm (LET a DUP2, _:x:_)          = (LET a (VAR x),l)
    rm (LET a DUP2, x:_)            = (LET a (VAR $ Arg 1),l)
    rm (LET a DUP2, _)              = (LET a (VAR $ Arg 2),l)
    rm (LET a DUP3, _:_:x:_)        = (LET a (VAR x),l)
    rm (LET a DUP3, _:x:_)          = (LET a (VAR $ Arg 1),l)
    rm (LET a DUP3, x:_)            = (LET a (VAR $ Arg 2),l)
    rm (LET a DUP3, _)              = (LET a (VAR $ Arg 3),l)
    rm (LET a DUP4, _:_:_:x:_)      = (LET a (VAR x),l)
    rm (LET a DUP4, _:_:x:_)        = (LET a (VAR $ Arg 1),l)
    rm (LET a DUP4, _:x:_)          = (LET a (VAR $ Arg 2),l)
    rm (LET a DUP4, x:_)            = (LET a (VAR $ Arg 3),l)
    rm (LET a DUP4, _)              = (LET a (VAR $ Arg 4),l)
    rm (LET a DUP5, _:_:_:_:x:_)    = (LET a (VAR x),l)
    rm (LET a DUP5, _:_:_:x:_)      = (LET a (VAR $ Arg 1),l)
    rm (LET a DUP5, _:_:x:_)        = (LET a (VAR $ Arg 2),l)
    rm (LET a DUP5, _:x:_)          = (LET a (VAR $ Arg 3),l)
    rm (LET a DUP5, x:_)            = (LET a (VAR $ Arg 4),l)
    rm (LET a DUP5, _)              = (LET a (VAR $ Arg 5),l)
    rm t                            = t 


rmSWAPs :: UAST -> UAST 
rmSWAPs tree = rmSWAPT tree tree 
    where 
    rmSWAPT :: UAST -> UAST -> UAST 
    rmSWAPT (BLK b bs)                      tree  = rmSWAPF (bs) tree                    
    rmSWAPT (RED a as)                      tree  = rmSWAPF (as) tree 
    rmSWAPF :: [UAST] -> UAST -> UAST 
    rmSWAPF []                              tree  = tree  
    rmSWAPF (BLK(UNIT SWAP1,a:b:_    )_:ts) tree  = rmSWAPF ts (swap tree a      b       tree) 
    rmSWAPF (BLK(UNIT SWAP1,a:_      )_:ts) tree  = rmSWAPF ts (swap tree a      (Arg 1) tree) 
    rmSWAPF (BLK(UNIT SWAP1,_        )_:ts) tree  = rmSWAPF ts (swap tree (Arg 1)(Arg 2) tree) 
    rmSWAPF (BLK(UNIT SWAP2,a:_:b:_  )_:ts) tree  = rmSWAPF ts (swap tree a      b       tree) 
    rmSWAPF (BLK(UNIT SWAP2,a:_:_    )_:ts) tree  = rmSWAPF ts (swap tree a      (Arg 1) tree) 
    rmSWAPF (BLK(UNIT SWAP2,a:_      )_:ts) tree  = rmSWAPF ts (swap tree a      (Arg 2) tree) 
    rmSWAPF (BLK(UNIT SWAP2,_        )_:ts) tree  = rmSWAPF ts (swap tree (Arg 1)(Arg 3) tree) 
    rmSWAPF (BLK(UNIT SWAP3,a:_:_:b:_)_:ts) tree  = rmSWAPF ts (swap tree a      b       tree) 
    rmSWAPF (BLK(UNIT SWAP3,a:_:_:_  )_:ts) tree  = rmSWAPF ts (swap tree a      (Arg 1) tree) 
    rmSWAPF (BLK(UNIT SWAP3,a:_:_    )_:ts) tree  = rmSWAPF ts (swap tree a      (Arg 2) tree) 
    rmSWAPF (BLK(UNIT SWAP3,a:_      )_:ts) tree  = rmSWAPF ts (swap tree a      (Arg 3) tree) 
    rmSWAPF (BLK(UNIT SWAP3,_        )_:ts) tree  = rmSWAPF ts (swap tree (Arg 1)(Arg 4) tree) 
    rmSWAPF (t                         :ts) tree  = rmSWAPF ts (rmSWAPT t tree)

swap :: UAST -> Var -> Var -> UAST -> UAST 
swap = swapT 
    where 
    swapT (RED(LET x a',us)ts)           a b tr | x==a        = RED (LET x (replaceO a b b'),us')(replaceF a b ts') where  RED (LET x' b',us')ts' = search tr b 
    swapT (RED(LET x b',us)ts)           a b tr | x==b        = RED (LET x (replaceO b a a'),us')(replaceF b a ts') where  RED (LET x' a',us')ts' = search tr a 
    swapT (RED t           ts)           a b tr               = RED t             (swapF ts a b tr) 
    swapT (BLK t           ts)           a b tr               = BLK t             (swapF ts a b tr) 
    swapF (BLK(UNIT SWAP1,x:y:_    )_:ts)a b tr | a==x&&b==y  =                    swapF ts a b tr
    swapF (BLK(UNIT SWAP1,x:_      )_:ts)a b tr | a==x        =                    swapF ts a b tr
    swapF (BLK(UNIT SWAP2,x:_:y:_  )_:ts)a b tr | a==x&&b==y  =                    swapF ts a b tr
    swapF (BLK(UNIT SWAP2,x:_:_    )_:ts)a b tr | a==x        =                    swapF ts a b tr
    swapF (BLK(UNIT SWAP3,x:_:_:y:_)_:ts)a b tr | a==x&&b==y  =                    swapF ts a b tr
    swapF (BLK(UNIT SWAP3,x:_:_:_  )_:ts)a b tr | a==x        =                    swapF ts a b tr
    swapF (t:ts) a b tr                                       = swapT t a b tr   : swapF ts a b tr
    swapF []     a b tr                                       = [] 


replaceO :: Var -> Var -> OPCODE -> OPCODE 
replaceO a b (VAR x) = if x==a then VAR b else VAR x  
replaceO a b o       = o 
replaceF :: Var -> Var -> [UAST] -> [UAST] 
replaceF a b [] = []
replaceF a b (RED(LET x o,us)os : ts) = RED (LET x(replaceO a b o),us) (replaceF a b os) : replaceF a b ts 
replaceF a b (BLK(UNIT  o,us)os : ts) = BLK (UNIT                o,us) (replaceF a b os) : replaceF a b ts 


rmSTACKs :: UAST -> UAST
rmSTACKs = fmap rm where
    rm  (LET x (STACK n), uncles) = (LET x (VAR(nth' n uncles)), uncles) where 
        
        nth' n []     = Arg n 
        nth' 1 (u:us) = u 
        nth' n (u:us) = nth' (n-1) us
    rm  x                                                = x 


rmVARs  :: UAST -> UAST 
rmVARs tree = rmT tree tree where 
    rmT tree (RED(LET y(VAR x),_)_)  = RED (LET y x',us') (rmF tree ts') where RED (LET _ x', us') ts' = search tree x
    rmT tree (RED a as)              = RED a              (rmF tree as) 
    rmT tree (BLK b bs)              = BLK b              (rmF tree bs)
    rmF tree (x:xs)                  = rmT tree x: rmF tree xs 
    rmF _ _                          = [] 


search :: UAST -> Var -> UAST  
search tree x = case searchT tree x of 
                    Just a  -> a 
                    Nothing -> BLK (UNIT $ UNDEFINED"",[]) []
    where
    searchT _ (Arg n)                       = Just $ RED (LET (Arg n) (VAR $ Arg n), []) []
    searchT (RED(LET x o, us)as) a | a == x = Just $ RED (LET x o, us) as 
    searchT (RED(LET x o, us)as) a          = searchF as a 
    searchT (BLK(UNIT  o, us)as) a          = searchF as a 
    searchF []                   a          = Nothing 
    searchF (x:xs)               a          = case searchT x a of 
                                                    Nothing -> searchF xs a 
                                                    Just a  -> Just a                 
    

