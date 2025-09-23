module Eval where 


import AST
import Term
import Type
import Tree
import Bind 
import Typing 






eval :: Ctx -> Ctx -> Term -> Term 
eval ctx stx tr = tr 




double f x = f (f x) 

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
        
{--
processDecls :: Ctx -> Ctx -> UVar -> Constraint -> [Decl] -> ([Decl], Ctx, UVar, Constraint) 
processDecls ctx stx q constr ds = case ds of 
    []                      -> ([], ctx, q, constr) 
    FLET id ps _ tr f : ds  -> (d':ds', ctx'', q'', constr'') where 
        tyF                         = TyID (var q) 
        _ctx                        = addBind ctx id (BindTmVAR tyF) 
        (_ps,_q,__ctx)              = addParamBind _ctx ps (q+1) 
        (t, ty, ___ctx, q', constr')= processTerm __ctx stx _q constr tr 
        sol                         = unify constr'
        ps'                         = double (apply_constr_params sol) _ps
        tyF'                        = paramret2ty ps' ty
        ctx'                        = addBind ctx id (BindTmVAR tyF')  
        d'                          = FLET id ps' ty t f  
        (ds', ctx'', q'', constr'') = processDecls ctx'' stx q' constr' ds
    LET  id    _ tr f : ds  -> (d':ds', ctx'', q'', constr'') where 
        tyR                         = TyID (var q) 
        _ctx                        = addBind ctx id (BindTmVAR tyR)
        (t, ty,__ctx, q', constr')  = processTerm _ctx stx (q+1) constr tr 
        ctx'                        = addBind ctx id (BindTmVAR ty) 
        d'                          = LET id     ty t f 
        (ds',ctx'', q'', constr'')  = processDecls ctx' stx q' constr' ds
    SLET id    _ tr f : ds  -> (d':ds', ctx'', q'', constr'') where  
        tyS                         = TyID (var q) 
        _stx                        = addBind stx id (BindTmVAR tyS)
        (t, ty, ctx', q', constr')  = processTerm ctx _stx (q+1) constr tr 
        stx'                        = addBind stx id (BindTmVAR ty) 
        d'                          = SLET id ty t f 
        (ds',ctx'', q'', constr'')  = processDecls ctx' stx' q' constr' ds
--}

processBody ctx stx q constr body = 
    let (body', tyT, q', constr')  = reconBODY ctx stx q body in 
    let constr''            = constr ++ constr' in 
    let sol                 = unify constr' in 
    let tyT'                = double (apply_constr sol) tyT in 
    (body', tyT', ctx, q', constr'') 


processTerm :: Ctx -> Ctx -> UVar -> Constraint -> Term -> (Term, Ty, Ctx, UVar, Constraint)    
processTerm ctx stx q constr tr = 
    let (tyT, q', constr')  = recon ctx stx q tr    in 
    let tr'                 = eval  ctx stx tr      in 
    let constr''            = constr ++ constr' in 
    let sol                 = unify constr'' in 
    let tyT'                = double (apply_constr sol) tyT in 
    (tr', tyT', ctx, q', constr'') 



fst5 (a,_,_,_,_) = a 

typingTest :: CONTRACT -> (CONTRACT, Ctx, Ctx, UVar, Constraint) 
typingTest cn = processCN [] [] 0 [] cn  

typing = fst5 . typingTest
