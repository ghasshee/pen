{-# LANGUAGE FlexibleInstances #-} 
module AST where 

import GCLL
import Type
import Term
import Tree
import Datatype
--import CPS 
    


data Decl   = FLET ID [Param] Term (Maybe Formulae)
            |  LET ID         Term (Maybe Formulae) 
            | SLET ID         Term (Maybe Formulae) 
--            | DATA ID [ID] [DConstr] 
            deriving (Eq, Read)  

instance Show Decl where 
    show (FLET i ps t p) = "FLET " ++ i ++ " " ++ show ps ++ " := " ++ show t ++ show p 
    show (SLET i    t p) = "SLET " ++ i ++ " "            ++ " := " ++ show t ++ show p  
    show ( LET i    t p) = " LET " ++ i ++ " "            ++ " := " ++ show t ++ show p 
--    show (DATA i is c)   = "DATA " ++ i ++ " " ++ show is ++ " := " ++ show c          

data BODY   = BODY (Maybe Formulae) [Decl] Term (Maybe Formulae) 
            deriving (Eq, Read, Show ) 


data BD  = BD (Maybe Formulae) Term (Maybe Formulae) 





-- LET x = t1 in t2 
-- == (ABS x t2 ) t1 
-- FLET f x = t1 in t2 
-- == LET f = FIX f x t1 in t2   
-- == (ABS f t2) (FIX f x t1) 
--SLET x = t1 in t2 
--SSTORE x t1; t2 
--
--e.g. 
-- SLET x = t1 in SLET x = x * 2 in t3 
-- == SSTORE x t1; (SLET x = (SLOAD x) * 2 in t3) 
-- == SSTORE x t1; (SSTORE x (SLOAD x * 2) ; t3 )   
--
--SLET x = 1 in LET y = x + 1 in SLET x = y * 2 in x + 3
--SSTORE x 1; (ABS y (SLET x = y * 2 in x + 3)) (SLOAD x + 1) 
--SSTORE x 1; (ABS y (SSTORE x (y * 2); SLOAD x + 3)) (SLOAD x + 1)  
--SSTORE x 1; (SSTORE x ((SLOAD x + 1) * 2); SLAOD x + 3) 
--SSTORE x 1; SSTORE x ((SLOAD x + 1) * 2); SLOAD x + 3 
--
--SLET x = 1 in LET f y = if y == 0 then 1 else x * f(y-1) in SLET x = f 3 in x + 3
--SSTORE x 1; (FLET f y (if y==0 then 1 else (SLOAD x) * f(y-1)) in SLET x = f 3 in x + 3)
--SSTORE x 1; (ABS f (SLET x = f 3 in x + 3)) (FIX f y (if y==0 then 1 else (SLOAD x)*f(y-1)))
--SSTORE x 1; (ABS f (SSTORE x (f 3); x + 3)) (FIX f y (if y==0 then 1 else (SLOAD x)*f(y-1)))
--SSTORE x 1; SSTORE x ((FIX f y (if y==0 then 1 else (SLOAD x)*f(y-1))) 3) ; SLOAD x + 3
--SSTORE x 1; SSTORE x (SLOAD x * SLOAD x * SLOAD x * 1); SLOAD x + 3


-- a METHOD is a sequence of tasks  
-- a function cannot overwrite storage 
-- 'overwrite' is a task 



data TOP    = MT ID Ty [Param] BODY     -- Method Definition 
--            | MT' ID Ty [Param] Term 
            | SV ID Ty                  -- Storage Variables 
            | EV ID Ty                  -- Event Declaration 
            | DT ID [ID] [DConstr]      -- Datatype Declaration 
            deriving (Show, Eq, Read) 





data CONTRACT 
            = CN ID [TOP] 
            deriving (Show, Eq, Read) 

instance {-# OVERLAPPING #-} Show [TOP] where 
    show []         = "\n\n"
    show (t:ts)     = "\n\n" ++ show t ++ show ts  


instance {-# OVERLAPPING #-} Show [Decl] where 
    show []         = "" 
    show (d:ds)     = "\n" ++ show d ++ " IN\n" ++ show ds 

method_call :: TOP -> CONTRACT 
method_call (MT id ty args body) = undefined 






-- || FOLD over AST || -- 


foldBODY _BD _Dc _Tm (BODY f1 ds t f2) = _BD (BODY f1 (foldDecl _Dc _Tm <$> ds) (_Tm t) f2) 

foldDecl (_F,_L,_S,_D) _Term (FLET i ps t p) = _F (FLET i ps (_Term t) p) 
foldDecl (_F,_L,_S,_D) _Term ( LET i    t p) = _L ( LET i    (_Term t) p) 
foldDecl (_F,_L,_S,_D) _Term (SLET i    t p) = _S (SLET i    (_Term t) p) 

foldTOP (_M,_S,_E,_D) _P _BD _Dc _Tm (SV i t     ) = _S(SV i t ) 
foldTOP (_M,_S,_E,_D) _P _BD _Dc _Tm (EV i t     ) = _E(EV i t ) 
foldTOP (_M,_S,_E,_D) _P _BD _Dc _Tm (DT i id ds ) = _D(DT i id ds) 
foldTOP (_M,_S,_E,_D) _P _BD _Dc _Tm (MT i t ps b) = 
    _M(MT i t(_P<$>ps)(foldBODY _BD _Dc _Tm b))

foldCONTRACT _TOP _P _BD _Dc _Tm (CN id tops)      = CN id (foldTOP _TOP _P _BD _Dc _Tm<$>tops) 



-- || FOLD contract only on Term || -- 
foldCONTRACT_Tm  _Tm cn = foldCONTRACT id4 id id id4 _Tm cn 

id4 = (id,id,id,id) 
