module PsrCtx where 

import Type 
import Term
import AST

type Var = ID
type Sto = ID 
type PsrCtx = ([Sto],[Var])  

data WhichVar = STO | VAR


emptyCtx :: PsrCtx
emptyCtx = ([],[]) 

addCtx :: WhichVar -> ID -> PsrCtx -> PsrCtx
addCtx VAR i (ss,vs) = (ss, i:vs)
addCtx STO i (ss,vs) = (i:ss, vs) 


mapStoTy    :: Ty -> [ID] -> PsrCtx -> ([TOP], PsrCtx) 
mapStoTy ty []     ctx      = ([], ctx)  
mapStoTy ty (i:is) ctx      = (SV i ty:svs, ctx') 
    where  (svs, ctx')          = mapStoTy ty is (addCtx STO i ctx)

mapParamTy  :: Ty -> [ID] -> PsrCtx -> ([Param], PsrCtx)
mapParamTy ty []     ctx    = ([], ctx)
mapParamTy ty (i:is) ctx    = ((i,ty):params, ctx') 
    where  (params, ctx')       = mapParamTy ty is (addCtx VAR i ctx) 





lookup :: ID -> PsrCtx -> Tm
lookup id (ss,vs) = loopVar vs 0 where 
    loopVar [] _        = loopSto ss 0 
    loopVar (x:xs) n    = if x == id then TmVAR n else loopVar xs (n+1) 
    loopSto [] _        = error $ "Variable " ++ id ++ " is Not Defined." 
    loopSto (s:ss) n    = if s == id then TmSTO n else loopSto ss (n+1) 


lookup' :: ID -> PsrCtx -> Maybe Tm 
lookup' id (ss,vs) = loopVar vs 0 where 
    loopVar [] _        = loopSto ss 0
    loopVar (x:xs) n    = if x == id then Just (TmVAR n) else loopVar xs (n+1)
    loopSto [] _        = Nothing 
    loopSto (s:ss) n    = if s == id then Just (TmSTO n) else loopSto ss (n+1)
