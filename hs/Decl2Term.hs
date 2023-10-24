module Decl2Term where 


import Tree
import Type
import Term
import AST
import Typing


transpileCN :: CONTRACT -> Term 
transpileCN (CN id [])      = undefined 
transpileCN (CN id tops)    = RED (TmCN id) (transpileTOPs tops)

transpileTOPs :: [TOP] -> [Term] 
transpileTOPs []            = []
transpileTOPs (top:tops)    = case top of 
    (MT id ty ags body)  -> RED (TmMT id (params2ty ags ty) ags) [transpileBODY body] : transpileTOPs tops
    (SV id ty)           -> [ RED (TmSLET id ty) (transpileTOPs tops) ] 
    (EV id ty)           -> undefined 


transpileBODY :: BODY -> Term 
transpileBODY (BODY _ ds t _) = transpileDecls ds t

transpileDecls :: [Decl] -> Term -> Term 
transpileDecls []                            t   = t
transpileDecls (SLET id     t formulae : ds) t'  = RED (TmSLET id (typeof t)) (t: [transpileDecls ds t']) 
transpileDecls ( LET id     t formulae : ds) t'  = RED (TmLET  id (typeof t)) (t: [transpileDecls ds t']) 
transpileDecls (FLET id ags t formulae : ds) t'  = RED (TmFLET id (params2ty ags (typeof t)) ags) (t: [transpileDecls ds t'])
transpileDecls (DATA id ids constrs    : ds) t'  = undefined   

params2ty :: [Param] -> Ty -> Ty 
params2ty []          rety  = rety
params2ty ((i,ty):ps) rety  = TyABS ty (params2ty ps rety)







