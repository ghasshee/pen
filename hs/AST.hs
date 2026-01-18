{-# LANGUAGE FlexibleInstances #-} 
module AST where 

import GCLL
import Type
import Param
import Term
import Tree
import Data
--import CPS 
    



data Decl   = FLET ID [Param] Ty Term (Maybe Formulae)
            |  LET ID         Ty Term (Maybe Formulae) 
            | SLET ID         Ty Term (Maybe Formulae) 
            deriving (Eq, Read)  

instance Show Decl where 
    show (FLET i ps ty t p) = "FLET " ++ i ++ " " ++ show ps ++ " : " ++ show ty ++ " := " ++ show t ++ show p 
    show (SLET i    ty t p) = "SLET " ++ i ++ " "            ++ " : " ++ show ty ++ " := " ++ show t ++ show p  
    show ( LET i    ty t p) = " LET " ++ i ++ " "            ++ " : " ++ show ty ++ " := " ++ show t ++ show p 

data BODY   = BODY (Maybe Formulae) [Decl] Term (Maybe Formulae) 
            deriving (Eq, Read, Show ) 

data TOP    = MT ID Ty [Param] BODY     -- Method Definition 
            | SV ID Ty                  -- Storage Variables 
            | SM ID Ty                  -- Storage Mappings 
            | EV ID Ty                  -- Event Declaration 
            | DT ID [Ty] [ID] [DConstr] -- Datatype Declaration 
            deriving (Eq, Read) 

instance Show TOP where 
    show (MT id ty ps bd    ) = "MT " ++ id ++ " " ++ show ty ++ " " ++ show ps ++ " " ++ show bd 
    show (SV id ty          ) = "SV " ++ id ++ " " ++ show ty 
    show (DT id tys ids cs  ) = "DT " ++ id ++ " " ++ show ids ++ " \n\t := " ++ show cs ++ "\n\t :: " ++ show tys

data CONTRACT = CN ID [TOP] 
            deriving (Show, Eq, Read) 

instance {-# OVERLAPPING #-} Show [DConstr] where 
    show []         = "\n"
    show [c]        = show c ++ "\n"  
    show (c:cs)     = show c ++ "\n\t |  " ++ show cs 

instance {-# OVERLAPPING #-} Show [TOP] where 
    show []         = "\n\n"
    show (t:ts)     = "\n\n" ++ show t ++ show ts  

instance {-# OVERLAPPING #-} Show [Decl] where 
    show []         = "" 
    show (d:ds)     = "\n" ++ show d ++ " IN\n" ++ show ds 







-- LET x = t1 in t2     ==>  (ABS x t2 ) t1 
-- FLET f x = t1 in t2  ==>  LET f = FIX f x t1 in t2  ==> (ABS f t2) (FIX f x t1) 
-- SLET x = t1 in t2    ==>  SSTORE x t1; t2 


--e.g. 
-- SLET x = t1 in SLET x = x * 2 in t3 
-- ==> SSTORE x t1; (SLET x = (SLOAD x) * 2 in t3) 
-- ==> SSTORE x t1; (SSTORE x (SLOAD x * 2) ; t3 )   
--
-- SLET x = 1 in LET y = x + 1 in SLET x = y * 2 in x + 3
-- ==> SSTORE x 1; (ABS y (SLET x = y * 2 in x + 3)) (SLOAD x + 1) 
-- ==> SSTORE x 1; (ABS y (SSTORE x (y * 2); SLOAD x + 3)) (SLOAD x + 1)  
-- ==> SSTORE x 1; (SSTORE x ((SLOAD x + 1) * 2); SLAOD x + 3) 
-- ==> SSTORE x 1; SSTORE x ((SLOAD x + 1) * 2); SLOAD x + 3 
--
-- SLET x = 1 in LET f y = if y == 0 then 1 else x * f(y-1) in SLET x = f 3 in x + 3
-- ==> SSTORE x 1; (FLET f y (if y==0 then 1 else (SLOAD x) * f(y-1)) in SLET x = f 3 in x + 3)
-- ==> SSTORE x 1; (ABS f (SLET x = f 3 in x + 3)) (FIX f y (if y==0 then 1 else (SLOAD x)*f(y-1)))
-- ==> SSTORE x 1; (ABS f (SSTORE x (f 3); x + 3)) (FIX f y (if y==0 then 1 else (SLOAD x)*f(y-1)))
-- ==> SSTORE x 1; SSTORE x ((FIX f y (if y==0 then 1 else (SLOAD x)*f(y-1))) 3) ; SLOAD x + 3
-- ==> SSTORE x 1; SSTORE x (SLOAD x * SLOAD x * SLOAD x * 1); SLOAD x + 3


-- a METHOD is a sequence of tasks  
-- a function cannot overwrite storage, that is, it's pure. 
-- 'overwrite' is a task 











-- || FOLD over AST || -- 



foldDecl (_F,_L,_S,_D) _Term (FLET i ps ty t p) = _F (FLET i ps ty (_Term t) p) 
foldDecl (_F,_L,_S,_D) _Term ( LET i    ty t p) = _L ( LET i    ty (_Term t) p) 
foldDecl (_F,_L,_S,_D) _Term (SLET i    ty t p) = _S (SLET i    ty (_Term t) p) 

foldBODY _BD _Dc _Tm (BODY f1 ds t f2) = _BD (BODY f1 (foldDecl _Dc _Tm <$> ds) (_Tm t) f2) 

foldTOP (_M,_S,_E,_D) _P _BD _Dc _Tm (SV i t     )   = _S(SV i t ) 
foldTOP (_M,_S,_E,_D) _P _BD _Dc _Tm (EV i t     )   = _E(EV i t ) 
foldTOP (_M,_S,_E,_D) _P _BD _Dc _Tm (DT i t id ds ) = _D(DT i t id ds) 
foldTOP (_M,_S,_E,_D) _P _BD _Dc _Tm (MT i t ps b)   = 
    _M(MT i t(_P<$>ps)(foldBODY _BD _Dc _Tm b))

foldCN  _TOP _P _BD _Dc _Tm (CN id tops)      = CN id (foldTOP _TOP _P _BD _Dc _Tm<$>tops) 



-- || FOLD contract only on Term || -- 
foldCN_Tm  _Tm cn = foldCN id4 id id id4 _Tm cn 

id4 = (id,id,id,id) 
