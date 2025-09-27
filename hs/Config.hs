module Config where 


import Type 
import Param
import Node
import Utils



-- || PROGRAM GRAPH || --
-- type Edge = (Node, Action, Node) 


-- CONFIGURE 
type FreshNode  = Int
type FreshSto   = Int 
type FreshVar   = Int
type INode      = Node Int
type TNode      = Node Int 

{-- Function Context consits of 3 components 
 -  1. The number of Args
 -  2. Initial Node of Program Graph of the function 
 -  3. Terminal Node of Program Graph of the function --} 

data FunOrArg a = Fun a | Arg a deriving (Functor, Show) 
type Var        = FunOrArg (Arity, INode, TNode)
type VarCtx     = [[Var]] 
type StoCtx     = [ID] 

-- Variables are contained in the reverse order i.e. 
-- given a function (F X Y = term) , de Bruijn Var is as follows; 
-- F : VAR 2 
-- X : VAR 1 
-- Y : VAR 0 
-- the variables are ordered by the VAR n, 
-- so the context is [Y,X,F] 

type Config     = (INode, TNode, FreshNode, FreshSto, FreshVar, VarCtx, StoCtx) 
type Config'    = (INode, TNode, FreshNode, FreshSto, FreshVar, VarCtx, StoCtx, Depth)

type Depth      = Int 


-- Storage assignment 
-- S[0] := 1 + S[0] 
-- produces code 
--      (TmSto 2) := 1 + (TmSto 0); 
--      (TmSto 0) := TmSto 2
-- in order that 
--      we want to distinguish pre/post values of storage. 
--
-- "reduced" removes this kind of redunduncy from StoCtx   
reduced :: StoCtx   -> StoCtx 
reduced []                     = [] 
reduced (('\'':_) : xs)        = reduced xs 
reduced stx                    = stx 






-- VAR n |-> 
-- 1. dive in corresponding function context (loop)  
-- 2. calculate VAR n is k-th argment of the context 
-- 3. note that we do not count higher-order arguments. 
calc_arglen :: VarCtx -> Int -> Int 
calc_arglen ctx n = calc (loop ctx n (hd ctx)) where 
    loop          []  _  _      = error $ "calc_arglen: ctx := " ++ show ctx 
    loop         xss  0  cur    = cur 
    loop (   [] :xss) n  cur    = loop     xss   n   (hd xss) 
    loop ((x:xs):xss) n  cur    = loop (xs:xss) (n-1) cur 
    calc              []        = error $ "calc_arglen: cannot reach here"
    calc (Fun _      :xs)       = 0
    calc (Arg (0,_,_):xs)       = 1 + calc xs
    calc (Arg (-1,_,_):xs)      = 1 + calc xs 
    calc (Arg _      :xs)       = calc xs


searchFun :: Int -> VarCtx -> Var 
searchFun i ctx     = loop i ctx where 
    loop 0 ([]:css)     = loop' 0 css
    loop 0 (cs:css)     = hd cs  
    loop n ([]:css)     = loop' n css
    loop n (cs:css)     = loop (n-1) (tl cs:css) 
    loop' 0 (cs:css)    = last cs  
    loop' n (cs:css)    = loop' (n-1) css
    loop' n []          = error $ "searchFun : out of context : " ++ show i ++ " : " ++ show ctx


searchSto :: ID -> StoCtx -> Maybe Int
searchSto s []                      = Nothing
searchSto s (x:xs)  | s == x        = Just (len xs)  
                    | otherwise     = searchSto s xs


searchArgs  i ctx   = loop i ctx where 
    loop  0 ([]:css)    = loop' 0 css
    loop  0 (cs:css)    = drop 1 $ reverse (hd ctx)  
    loop  n ([]:css)    = loop' n css
    loop  n (cs:css)    = loop (n-1) (tl cs:css) 
    loop' 0 (cs:css)    = if len cs <= 1 then err ("searchArgs " ++ show cs) else tl $ reverse cs
    loop' n (cs:css)    = loop' (n-1) css
    loop' n []          = err $ "searchFun : out of context : " ++ show i ++ " : " ++ show ctx





