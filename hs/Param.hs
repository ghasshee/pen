module Param where 


import Type 


type Param  = (ID, Ty) 
type Params = [Param] 

showTyParam (id,ty) = showTy ty 
showTyParams ps = "(" ++ loop ps where 
    loop []     = ")"
    loop [p]    = showTyParam p ++ ")" 
    loop (p:ps) = showTyParam p ++ "," ++ loop ps  




-- Arity of Parameters 

type Arity = Int 
type ParamArity  = (ID, Arity) 
type ParamsArity = [ParamArity] 

paramArities    :: Params -> ParamsArity 
paramArities ps     = (tyArity <$>) <$> ps 

tyArity       :: Ty -> Arity
tyArity (TyARR _ b)         = tyArity b + 1 
tyArity _                   = 0  


