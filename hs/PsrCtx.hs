module PsrCtx where 

import Type 
import Term
import AST

type PsrVar = String
type StoArg = String 
type PsrCtx = ([StoArg],[PsrVar])  
data WhichCtx = Sto | Psr 


emptyCtx :: PsrCtx
emptyCtx = ([],[]) 

addCtx :: WhichCtx -> ID -> PsrCtx -> PsrCtx
addCtx Psr i (stovars,psrvars) = (stovars,i:psrvars)
addCtx Sto i (stovars,psrvars) = (i:stovars,psrvars) 


-- contract foo {
--     s1 : S1
--     s2 : S2  
--    ...
--  }       ==>  ctx  |->  (s1:s2:ctx)   -- Order Preserved 
-- mapStoTy :: Ty -> [String] -> a -> a 
mapStoTy :: Ty -> [String] -> PsrCtx -> ([TOP], PsrCtx) 
mapStoTy ty []     ctx = ([], ctx)  
mapStoTy ty (i:is) ctx = (SV i ty:svs, ctx') 
    where  (svs, ctx') = mapStoTy ty is (addCtx Sto i ctx)

mapParamTy :: Ty -> [String] -> PsrCtx -> ([Param], PsrCtx)
mapParamTy ty []     ctx  = ([], ctx)
mapParamTy ty (i:is) ctx  = ((i,ty):params, ctx') 
    where  (params, ctx')  = mapParamTy ty is (addCtx Psr i ctx) 





lookup :: ID -> PsrCtx -> Tm
lookup id (sto,psr) = loopPsr psr 0 where 
    loopPsr [] _        = loopSto sto 0 
    loopPsr (p:ps) n    = if p == id then TmVAR n else loopPsr ps (n+1) 
    loopSto [] _        = error $ "Variable " ++ id ++ " is Not Defined." 
    loopSto (s:ss) n    = if s == id then TmSTO n else loopSto ss (n+1) 

