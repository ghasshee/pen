module Datatype where 

import Hex
import Type 
import LTree
import Utils 
import Semiring 





-- Inductive Data Type 
data DInd       = DInd ID [ID] [DConstr] 

data DConstr    = DConstr ID [Ty] 
                deriving (Show, Eq, Read) 



dInd2FTy (DInd id args [c])    = dConstr2FTy id args c 
dInd2FTy (DInd id args (c:cs)) =  
    FSum (dConstr2FTy id args c) (dInd2FTy (DInd id args cs))

dConstr2FTy id args (DConstr cid []      ) = FOne 
dConstr2FTy id args (DConstr cid [ty]    ) = ty2FTy id args ty 
dConstr2FTy id args (DConstr cid (ty:tys)) = 
    FProd (ty2FTy id args ty) (dConstr2FTy id args (DConstr cid tys))


ty2FTy id args (TyDATA s tys) | id == s = FVar "List" 
ty2FTy id args ty                       = FConst ty 



-- e.g. 
-- defining List 

l = DInd "List" ["a"] 
        [DConstr "Nil"  []
        ,DConstr "Cons" [TyID "a", TyDATA "List" [TyID "a"]]
        ] 


-- Functor Type f 
data FTy x  = FZero   
            | FOne 
            | FVar x   -- Recursive Parameter 
            | FProd (FTy x) (FTy x) 
            | FSum  (FTy x) (FTy x) 
            | FConst Ty  
            deriving (Eq) 


instance Functor FTy  where 
    fmap _ FZero = FZero 
    fmap _ FOne  = FOne 
    fmap f (FVar x) = FVar (f x) 
    fmap f (FProd a b) = FProd (fmap f a) (fmap f b) 
    fmap f (FSum a b)  = FSum (fmap f a) (fmap f b) 


instance Show (FTy String) where 
    show FZero          =   "0" 
    show FOne           =   "1" 
    show (FVar x)       = x 
    show (FProd(FSum a b)(FSum c d)) 
                        =   "(" ++ show(FSum a b) ++ ")" ++ " × " ++
                            "(" ++ show(FSum c d) ++ ")" 
    show (FProd(FSum a b)c)   
                        =   "(" ++ show(FSum a b) ++ ")" ++ " × " ++ show c
    show (FProd a(FSum b c)) 
                        =   show a ++ " × " ++ "(" ++ show (FSum b c) ++ ")"
    show (FProd a b)    =   show a ++ " × " ++ show b 
    show (FSum  a b)    =   show a ++ " + " ++ show b 
    show (FConst ty)    =   show ty 

instance Semigroup (FTy String)  where 
    a <> b              = FProd a b 
instance Monoid (FTy String)  where 
    mempty              = FOne 
instance Semiring (FTy String)  where 
    zero                = FZero 
    a <+> b             = FSum a b 
    iszero a            = a == FZero 

instance Num (FTy String) where 
    (+)     = (<+>) 
    (*)     = (<.>) 
    (-)     = undefined 
    abs     = undefined 
    signum  = undefined 
    fromInteger n = loop n where 
        loop 0 = FZero  
        loop 1 = FOne 
        loop n  | n`mod`2 == 0  = FProd (FSum FOne FOne) (loop (n`div`2))  
                | otherwise     = FSum FOne (loop (n - 1)) 



------------------------------------
---     memorize with FIX        ---
------------------------------------
    

data Fix f = In (f (Fix f))


cata :: Functor f => (f a -> a) -> Fix f -> a 
cata alg (In x) = alg (fmap (cata alg) x) 

show' :: String -> String 
show' "" = "" 
show' ('\\':  xs) = show' xs 
show' ('"' :  xs) = show' xs 
show' (x   :  xs) =  x : show' xs 

showFix :: Functor f => (f String -> String) -> Fix f -> String
showFix alg = cata alg 

instance Show (Fix FTy) where 
    show = show' . showFix (show) 

instance Show a => Show (Fix (ListF a)) where 
    show = show' . showFix (show) 

data ListF a x = NilF | ConsF a x 
    deriving (Show) 

instance Functor (ListF a) where 
    fmap _ NilF         = NilF 
    fmap f (ConsF a x)  = ConsF a (f x) 


type List a = Fix (ListF a) 

a = In (ConsF 1 (In (ConsF 2 (In (ConsF 3 (In NilF))))))


toList (In NilF) = [] 
toList (In (ConsF a as)) = a : toList as 






    {--
# solc 

# mapping 
# variable 
--}


data Patricia   = Undefined 

{--
type mapping = address -> uint 
--}
















{--
contract bank { 

method withdraw (amount : Wei) {

    data Tree  = Leaf amount 
               | Node account Tree Tree 


--}

data IndTree    = ILf ID 
                | ILfTy Ty
                | IBr [IndTree] 
                | IBrAll ID [IndTree] 
                | IBrEx  ID [IndTree] 
