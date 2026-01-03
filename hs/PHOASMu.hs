{-# LANGUAGE PatternSynonyms #-} 
module PHOASMu where 



data TyF a r    = Var a 
                | Lam ( a -> r) 
                | Rec ( a -> r ) 
                | App r r 
                | Lit Int 
                | Add r r
                deriving Functor 


newtype Mu f = In { out :: f (Mu f) } 

type Ty a = Mu (TyF a)


--pattern VarT :: a -> Ty a 
pattern VarT x      = In (Var x)
pattern LamT f      = In (Lam f) 
pattern RecT f      = In (Rec f)
pattern AppT f x    = In (App f x)
pattern LitT n      = In (Lit n) 
pattern AddT x y    = In (Add x y) 





