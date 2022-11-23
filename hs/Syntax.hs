module Syntax where 

type ID     =   String

data Ty     =   TyERR 
            |   TyUnit
            |   TyU256  -- 256 bits
            |   TyU8    --   8 bits
            |   TyB32   -- 256 bits 
            |   TyADR   -- 160 bits
            |   TyBOOL
            |   TyABS   Ty Ty 
            |   TyMAP   Ty Ty 
            |   TyMTHD  ID [Ty] Ty
            |   TyDFLT
            |   TyREF   Ty




data Top ty'    =   CN ID [Ty] (Mthd ty') 
                |   EV Ty  

data Mthd ty'   =   MT Ty (TmTy ty')

data TmTy ty'   =   TYPED (Tm ty') ty' 

data Tm ty'     =   TmAPP       -- TmAPP (A->B) A   ::  B 
                |   TmABS       -- TmABS (A->B)     ::  A -> B 
                |   TmFIX       -- TmFIX (A->A)     ::  A  
                |   TmU8        -- TmU8             ::  TyU8 
                |   TmRET       -- TmRET (A->Cont)->Cont  ::  A ? Cont ?    




{-- 
 -  f :  ([A] -> Cont) -> Cont
 -  f (\[]   -> stop             ) 
 -    (\x:xs -> x == 1 then end)   
 -
 -
 - --}
