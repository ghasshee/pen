module Decl2Term where 


import Tree
import Type
import Term
import AST 
import Typing


transpileCN :: CONTRACT -> AST 
transpileCN (CN id [])          = undefined 
transpileCN (CN id tops)  = RED (TmCN id) (transpileTOPs tops)

transpileTOPs :: [TOP] -> [AST] 
transpileTOPs [] = []
transpileTOPs (top:tops) = case top of 
    (MT id ty params pre body post)    -> RED (TmMT id (params2ty params ty) params) [transpileBODY body] : transpileTOPs tops
    (SV id ty)                         -> [ RED (TmSLET id ty) (transpileTOPs tops) ] 
    (EV id ty)                         -> undefined 


transpileBODY :: BODY -> AST 
transpileBODY (BODY ds t) = transpileDecls ds t

transpileDecls :: [Decl] -> AST -> AST 
transpileDecls [] t = t
transpileDecls (SLET id   t formulae     : ds)  t'  = RED (TmSLET id (typeof t)) (t: [transpileDecls ds t']) 
transpileDecls (LET id    t formulae     : ds)  t'  = RED (TmLET id (typeof t)) (t: [transpileDecls ds t']) 
transpileDecls (FLET id params t formulae : ds)  t' = RED (TmFLET id (params2ty params (typeof t)) params) (t: [transpileDecls ds t'])
transpileDecls (DATA id ids constrs      : ds)  t'  = undefined   

params2ty :: [Param] -> Ty -> Ty 
params2ty [] rety           = rety
params2ty ((i,ty):ps) rety  = TyABS ty (params2ty ps rety)







