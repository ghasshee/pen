{-# LANGUAGE UnicodeSyntax #-}

module CPS where 

import Tree 
import Term
import Type 


type CPS a = ∀ k . (a -> k) -> k 

type VOID = ∀ a. a

toCPS :: a -> CPS a 
toCPS x = ($ x) 


fromCPS :: CPS a -> a 
fromCPS x = x id 


cpsTy :: Ty -> Ty
cpsTy = undefined 

--shift :: Int -> Term -> Term 
shift k (RED (TmVAR i)[]) = RED (TmVAR (i+k)) [] 
shift k (RED TmAPP [t1,t2]) = RED TmAPP [shift k t1, shift k t2] 


eta f               = app f  
app x y             = RED TmAPP [x,y] 
var n               = RED (TmVAR n) [] 
if_ x y z           = RED TmIF [x,y,z] 
bop o x y           = RED (TmBOP o) [x,y] 
fix f args ty tm    = RED (TmFIX f args ty) [tm]  
kont k              = RED (TmKont (K k))[] 




cps(RED TmIF   [b,t1,t2])k = cps b (\x -> (if_ x(cps t1 k)(cps t2 k)))
cps(RED(TmBOP o) [t1,t2])k = cps t1(\x -> cps t2(\y -> k (bop o x y)))  
cps(RED TmAPP [t1,t2])   k = cps t1(\x -> cps t2(\y -> app (app x y) (kont k))) 
cps(RED(TmFIX f[x]ty)[t])k = k (fix f [x,"c"] ty' (cps t'(eta(var 0)))) where
                                t' = shift 1 t 
                                ty' = cpsTy ty 
cps x                    k = k x  






