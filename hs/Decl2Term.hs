module Decl2Term where 


import Tree
import Bind 
import Type
import Param 
import Term
import AST
import Typing


transpileCN :: CONTRACT -> Term 
transpileCN (CN id tops)    = RED (TmCN id) (transpileTOPs [] tops)

transpileTOPs :: Ctx -> [TOP] -> [Term] 
transpileTOPs ctx []            = []
transpileTOPs ctx (top:tops)    = case top of 
    (MT id ty ps body)     -> RED (TmMT id tys ps) trs : transpileTOPs ctx tops where 
        trs = [transpileBODY ctx body] 
        tys = params2ty ps ty 
    (SV id ty)              -> [ BLK (TmSLET id ty) (transpileTOPs ctx tops) ] 
    (EV id ty)              -> undefined 
    (DT id ty ids cnstrs)   -> undefined   


transpileBODY :: Ctx -> BODY -> Term 
transpileBODY ctx (BODY _ ds t _) = transpileDecls ctx ds t

transpileDecls :: Ctx -> [Decl] -> Term -> Term 
transpileDecls ctx []      t    = t
transpileDecls ctx (d:ds)  t'   = case d of  
    SLET id    _ t formulae   ->  BLK (TmSLET id (typeof ctx t)) (t: [transpileDecls ctx ds t']) 
    LET  id    _ t formulae   ->  RED (TmLET  id (typeof ctx t)) (t: [transpileDecls ctx ds t']) 
    FLET id ps _ t formulae   ->  RED (TmFLET id (params2ty ps (typeof ctx t)) ps) (t: [transpileDecls ctx ds t'])

params2ty :: [Param] -> Ty -> Ty 
params2ty []          rety  = rety
params2ty ((i,ty):ps) rety  = TyARR ty (params2ty ps rety)







