module Subtyping where 


import Type 
import Bind 
import Prelude hiding ((<$))


{--
isTyAbb :: Ctx -> ID -> Bool 
isTyAbb ctx i   = case getBind ctx i of 
    BindTyAbb _     -> True
    _               -> False 

getTyAbb :: Ctx -> ID -> Ty
getTyAbb ctx i  = case getBind ctx i of 
    BindTyAbb tyT   -> tyT
    _               -> error "getTyABB: cannot get type abbreviation" 

computeTy :: Ctx -> Ty -> Ty 
computeTy ctx (TyID i) = case getBind ctx i of 
    BindTyAbb tyT   -> tyT 
    _               -> TyID i 
computeTy ctx tyT       = tyT 
--} 
simplifyty :: Ctx -> Ty -> Ty 
simplifyty ctx tyT      = loop tyT  where 
    loop (TyID i)  = case getBind ctx i of 
        BindTyAbb tyT   -> loop tyT 
        _               -> error $ "simplifyty: cannot get TyID abbreviation" ++ show i 
    loop tyT        = tyT 

subtype = (<$) 

(<$) :: Ty -> Ctx -> Ty -> Bool 
(tyS <$ ctx) tyT = 
    let tyS' = simplifyty ctx tyS 
        tyT' = simplifyty ctx tyT in case (tyS', tyT') of 
        (_,TyTOP)                   -> True
        (TyARR s1 s2, TyARR t1 t2)  -> (t1 <$ ctx $ s1) && (s2 <$ ctx $ t2)
        (TyREF t1, TyREF t2)        -> (t1 <$ ctx $ t2) && (t2 <$ ctx $ t1)
        (TyREF t1, TySRC t2)        -> t1 <$ ctx $ t2 
        (TySRC t1, TySRC t2)        -> t1 <$ ctx $ t2 
        (TySINK s, TySINK t)        -> t  <$ ctx $ s 
        (TyREF  s, TySINK t)        -> t  <$ ctx $ s 
        (tyS     , tyT     )        -> False {-- #TODO --}

