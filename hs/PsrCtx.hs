module PsrCtx where 

import Type 
import Term
import AST

type PsrVar = (String,Ty)
type StoArg = (String,Ty)
type PsrCtx = ([StoArg],[PsrVar])  
data WhichCtx = Sto | Psr 


emptyCtx :: PsrCtx
emptyCtx = ([],[]) 

addCtx :: WhichCtx -> ID -> Ty -> PsrCtx -> PsrCtx
addCtx Psr i ty (stovars,psrvars) = (stovars,(i,ty):psrvars)
addCtx Sto i ty (stovars,psrvars) = ((i,ty):stovars,psrvars) 


-- contract foo {
--     s1 : S1
--     s2 : S2  
--    ...
--  }       ==>  ctx  |->  (s1:s2:ctx)   -- Order Preserved 
-- mapStoTy :: Ty -> [String] -> a -> a 
mapStoTy :: Ty -> [String] -> PsrCtx -> ([TOP], PsrCtx) 
mapStoTy ty []     ctx = ([], ctx)  
mapStoTy ty (i:is) ctx = (SV i ty:svs, addCtx Sto i ty ctx') 
    where  (svs, ctx') = mapStoTy ty is ctx 

mapParamTy :: Ty -> [String] -> PsrCtx -> ([Param], PsrCtx)
mapParamTy ty []     ctx  = ([], ctx)
mapParamTy ty (i:is) ctx  = ((i,ty):params, ctx'') 
    where  ctx'             = addCtx Psr i ty ctx 
           (params, ctx'')  = mapParamTy ty is ctx' 




