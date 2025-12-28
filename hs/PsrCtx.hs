module PsrCtx where 

import Type 
import Param
import Term
import AST

type Var  = ID
type Sto  = ID  
type Dta  = ID 
type PsrCtx = ([Sto],[Var], [Dta])  

data WhichVar = STO | VAR | DTA 

emptyCtx :: PsrCtx
emptyCtx = ([],[],[]) 

addCtx :: WhichVar -> ID -> PsrCtx -> PsrCtx
addCtx VAR i (ss,vs,ds) = (ss, i:vs,ds)
addCtx STO i (ss,vs,ds) = (i:ss, vs,ds) 
addCtx DTA i (ss,vs,ds) = (ss,vs, i:ds)


mapStoTy    :: Ty -> [ID] -> PsrCtx -> ([TOP], PsrCtx) 
mapStoTy ty []     ctx      = ([], ctx)  
mapStoTy ty (i:is) ctx      = (SV i ty:svs, ctx') 
    where  (svs, ctx')          = mapStoTy ty is (addCtx STO i ctx)

mapParamTy  :: Ty -> [ID] -> PsrCtx -> ([Param], PsrCtx)
mapParamTy ty []     ctx    = ([], ctx)
mapParamTy ty (i:is) ctx    = ((i,ty):params, ctx') 
    where  (params, ctx')       = mapParamTy ty is (addCtx VAR i ctx) 





lookup :: ID -> PsrCtx -> Tm
lookup id (ss,vs,ds) = loopVar vs 0 where 
    loopVar [] _        = loopDta ds 0 
    loopVar (x:xs) n    = if x == id then TmVAR n else loopVar xs (n+1) 
    loopDta [] _        = loopSto ss 0 
    loopDta (d:ds) n    = if d == id then TmCON id else loopDta ds (n+1) 
    loopSto [] _        = error $ "PsrCtx.hs: lookup: " ++ id ++ " is not passed to the context." 
    loopSto (s:ss) n    = if s == id then TmSTO n else loopSto ss (n+1) 


lookup' :: ID -> PsrCtx -> Maybe Tm 
lookup' id (ss,vs,ds) = loopVar vs 0 where 
    loopVar [] _        = loopDta ds 0
    loopVar (x:xs) n    = if x == id then Just (TmVAR n) else loopVar xs (n+1)
    loopDta [] _        = loopSto ss 0
    loopDta (d:ds) n    = if d == id then Just (TmCON id) else loopDta ds (n+1) 
    loopSto [] _        = Nothing 
    loopSto (s:ss) n    = if s == id then Just (TmSTO n) else loopSto ss (n+1)
