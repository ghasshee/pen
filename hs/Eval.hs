module Eval where 


import AST
import Term
import Type
import Tree
import Bind 
import Typing 
import Utils






eval :: Ctx -> Ctx -> Term -> Term 
eval ctx stx tr = tr 





type UVar = Int 




processCN :: Ctx -> Ctx -> UVar -> Constraint -> CONTRACT -> (CONTRACT, Ctx, Ctx, UVar, Constraint) 
processCN ctx stx q constr (CN id tops) = (CN id tops', ctx', stx', q', constr') where 
    (tops', ctx', stx',  q', constr')   = processTOPs ctx stx q constr tops

processTOPs :: Ctx -> Ctx -> UVar -> Constraint -> [TOP] -> ([TOP], Ctx, Ctx, UVar, Constraint) 
processTOPs ctx stx q constr tops = case tops of 
    []                                  -> ([], ctx, stx, q, constr) 
    SV id ty                      : ts  -> (SV id ty' : ts', ctx', stx'', q', constr') where 
        ty'                             = TyID (var q) 
        stx'                            = addBind stx id (BindTmVAR ty')
        (ts', ctx',stx'', q', constr')  = processTOPs ctx stx' (q+1) constr ts
    MT id ty ps body : ts  -> (m' : ts', ctx'',stx', q'', constr'') where
        tyR                             = TyID (var q) 
        tyM                             = paramret2ty ps tyR
        (body', tyR',ctx',q',constr')   = processBody ctx stx q  constr body
        m'                              = MT id tyR' ps body'  
        (ts', ctx'', stx', q'',constr'')= processTOPs ctx' stx q' constr' ts
        

processBody :: Ctx -> Ctx -> UVar -> Constraint -> BODY -> (BODY, Ty, Ctx, UVar, Constraint) 
processBody ctx stx q constr body = 
    let (body', tyT, q', constr')  = reconBODY ctx stx q body in 
    let constr''            = constr ++ constr' in 
    let sol                 = unify constr'' in 
    let tyT'                = double (apply_constr sol) tyT in 
    (body', tyT', ctx, q', constr'') 



fst5 (a,_,_,_,_) = a 

typingTest :: CONTRACT -> (CONTRACT, Ctx, Ctx, UVar, Constraint) 
typingTest cn = processCN [] [] 0 [] cn  

typing = fst5 . typingTest
