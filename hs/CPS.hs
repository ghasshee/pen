module CPS where 

import Syntax 
import Cont
import Effect 
import GCLL



toCPSTm :: Tm -> CPS Tm
toCPSTm (TmABS x ty t)  k   = toCPSTm t $ \m -> k (TmABS x ty m) 
toCPSTm (TmAPP t t')    k   = toCPSTm t $ \m -> toCPSTm t' $ \n -> k (TmAPP m n) 
toCPSTm (TmIF b t t')   k   = toCPSTm b $ \b' -> toCPSTm t $ \m -> toCPSTm t' $ \n -> k (TmIF b' m n) 
toCPSTm t               k   = toCPS t k 


facCPS 0 k = k 1 
facCPS n k = facCPS (n-1) $ k . (*n) 

fac 0 = 1 
fac n = n * fac(n-1) 

fac10  =  facCPS 10 (\x -> x)
fac10' =  facCPS 10 (\x -> print x) 



