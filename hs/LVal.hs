module  LVal  where 


import Term 
import Type 
import PsrCtx 

lookupLVAL :: String -> Ty -> PsrCtx -> (Tm, PsrCtx)  
lookupLVAL id ty (stoctx, psrctx) = case lookupPsr id ty psrctx of 
    Just tm     -> (tm, (stoctx,psrctx)) 
    Nothing     -> case lookupSto id ty stoctx of 
        Just tm     -> (tm, (stoctx,psrctx)) 
        Nothing     -> newVar id ty (stoctx, psrctx) 


lookupPsr :: String -> Ty -> [PsrVar] -> Maybe Tm
lookupPsr id ty psrctx = loop psrctx 0 where 
    loop []           _ = Nothing
    loop ((i,t):ctx)  n = if i==id && t == ty   
                                then Just (TmVAR n) 
                                else loop ctx (n+1) 

newVar :: String -> Ty -> PsrCtx -> (Tm, PsrCtx) 
newVar id ty (stoctx, psrctx) = (TmVAR $ length psrctx, (stoctx, (id,ty):psrctx))

lookupSto id ty stoctx = undefined 



