-- {-# LANGUAGE RankNTypes #-} 



-- Parametric Higher Order Abstract Syntax 
module PHOAS where 


import Control.Monad.State


data Exp v      = Var v
                | Lam (v -> Exp v) 
                | App (Exp v) (Exp v) 
            
instance Show (Exp String) where 
    show = show . showPHOAS 

data PExp       = PVar String
                | PInt Int 
                | PLam String PExp 
                | PApp PExp PExp
                deriving (Show) 


type Fresh = State Int 

fresh :: Fresh String
fresh = do 
    n <- get
    put (n+1) 
    pure ("x" ++ show n)


reify :: Exp String -> Fresh PExp
reify (Var v)   = pure $ PVar v
reify (App a b) = PApp <$> (reify a) <*> (reify b) 
reify (Lam f)   = do 
    x <- fresh 
    fx <- reify (f x) 
    pure $ PLam x fx 

showPHOAS e = 
    evalState (reify e) 0 



data Value      = VLam (Value -> Value)
                | VInt Int
                | VVar String
instance Show Value where 
    show v = show $ evalState (reifyV v) 0 


reifyV :: Value -> Fresh PExp
reifyV (VInt i) = pure $ PInt i 
reifyV (VVar x) = pure $ PVar x
reifyV (VLam f) = do 
    x  <- fresh 
    fx <- reifyV (f (VVar x)) 
    pure $ PLam x fx 
             




eval :: Exp Value -> Value 
eval (Var v)        = v 
eval (Lam f)        = VLam (\x -> eval (f x))
eval (App e1 e2)    = case eval e1 of 
                        VLam f -> f (eval e2)


type ClosedExp = forall v. Exp v 



idE :: ClosedExp 
idE = Lam (\x -> Var x) 


run :: ClosedExp -> Value 
run e = eval e 



example :: ClosedExp 
example = (App (Lam (\x -> Var x)) (Lam (\y -> Var y)))



