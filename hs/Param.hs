module Param where 


import Type 


type Param  = (ID, Ty) 
type Params = [Param] 

showTyParam (id,ty) = showTy ty 
showTyParams ps = "(" ++ loop ps where 
    loop []     = ")"
    loop [p]    = showTyParam p ++ ")" 
    loop (p:ps) = showTyParam p ++ "," ++ loop ps  

