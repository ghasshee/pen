letrec fac = \n. if iszero n then 1 else n * (fac(pred n)) in fac 5 ; 
