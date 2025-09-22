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








{--
data Decl   = FLET ID [Param] Ty Term (Maybe Formulae)
            |  LET ID         Ty Term (Maybe Formulae) 
            | SLET ID         Ty Term (Maybe Formulae) 
            deriving (Eq, Read)  

data BODY   = BODY (Maybe Formulae) [Decl] Term (Maybe Formulae) 
            deriving (Eq, Read, Show ) 

data TOP    = MT ID Ty [Param] BODY     -- Method Definition 
            | SV ID Ty                  -- Storage Variables 
            | SM ID Ty                  -- Storage Mappings 
            | EV ID Ty                  -- Event Declaration 
            | DT ID [ID] [DConstr]      -- Datatype Declaration 
            deriving (Show, Eq, Read) 

data CONTRACT 
            = CN ID [TOP] 
            deriving (Show, Eq, Read) 
--} 



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
    MT id ty ps (BODY p1 ds t p2) : ts  -> (m' : ts', ctx''',stx', q''', constr''') where
        tyR                             = TyID (var q) 
        tyM                             = paramret2ty ps tyR
        _ctx                            = addBind ctx id (BindTmVAR tyM) 
        (ds',ctx',q',constr')           = processDecls _ctx stx q  constr ds 
        (t',rety, ctx'', q'', constr'') = processTerm  ctx' stx q' constr' t
        sol                             = unify ctx'' constr'' 
        tyR'                            = apply_constr sol tyR 
        m'                              = MT id tyR' ps (BODY p1 ds' t' p2)  
        (ts', ctx''', stx', q''', constr''')  = processTOPs ctx'' stx q'' constr'' ts
        

paramret2ty :: [Param] -> Ty -> Ty 
paramret2ty ps rety = loop (reverse ps) rety where 
    loop []             ty = ty 
    loop ((_,t1):rest)  ty = TyARR t1 ty 

addParamBind ctx params q = case params of 
    []                      -> (params , q , ctx  ) 
    (p,Untyped) : rest      -> (params'', q', ctx'') where 
        tyP                     = TyID (var q) 
        ctx'                    = addBind ctx p (BindTmVAR tyP)
        params''                = (p,tyP) : params' 
        (params', q', ctx'')    = addParamBind ctx' rest (q+1)   
    (p,tyP) : rest          -> (params', q', ctx'') where 
        ctx'                    = addBind ctx p (BindTmVAR tyP)
        (params', q', ctx'')    = addParamBind ctx' rest q 

apply_constr_params constr params = case params of 
    []          -> []
    (id,ty):ps  -> (id, apply_constr constr ty) : apply_constr_params constr ps 

processDecls :: Ctx -> Ctx -> UVar -> Constraint -> [Decl] -> ([Decl], Ctx, UVar, Constraint) 
processDecls ctx stx q constr ds = case ds of 
    []                      -> ([], ctx, q, constr) 
    FLET id ps _ tr f : ds  -> (d':ds', ctx''', q'', constr'') where 
        tyR                         = TyID (var q) 
        _ctx                        = addBind ctx id (BindTmVAR tyR) 
        (_ps,_q,__ctx)              = addParamBind _ctx ps (q+1) 
        (t, ty, ctx', q', constr')  = processTerm __ctx stx _q constr tr 
        sol                         = unify ctx' constr'
        ps'                         = double (apply_constr_params sol) _ps
        tyF                         = paramret2ty ps' ty
        ctx''                       = addBind ctx' id (BindTmVAR tyF)  
        d'                          = FLET id ps' ty t f  
        (ds', ctx''', q'', constr'')= processDecls ctx'' stx q' constr' ds
    LET  id    _ tr f : ds  -> (d':ds', ctx''', q'', constr'') where 
        tyR                         = TyID (var q) 
        _ctx                        = addBind ctx id (BindTmVAR tyR)
        (t, ty, ctx', q', constr')  = processTerm _ctx stx (q+1) constr tr 
        ctx''                       = addBind ctx' id (BindTmVAR ty) 
        (ds',ctx''', q'', constr'') = processDecls ctx'' stx q' constr' ds
        d'                          = LET id     ty t f 
    SLET id    _ tr f : ds  -> (d':ds', ctx'', q'', constr'') where  
        tyS                         = TyID (var q) 
        stx'                        = addBind stx id (BindTmVAR tyS)
        (t, ty, ctx', q', constr')  = processTerm ctx stx' (q+1) constr tr 
        --stx''                       = rewriteBind stx' id (BindTmVAR ty) 
        (ds',ctx'', q'', constr'')  = processDecls ctx' stx' q' constr' ds
        d'                          = SLET id ty t f 


rewriteBind :: Ctx -> ID -> Bind -> Ctx 
rewriteBind stx x bind = case stx of 
    []                          -> error $ "rewriteBind: id " ++ show x ++ " not found" 
    (id,bd):bs  | x == id       -> (id,bind) : bs 
                | otherwise     -> (id,bd)   : rewriteBind bs x bind 

processTerm :: Ctx -> Ctx -> UVar -> Constraint -> Term -> (Term, Ty, Ctx, UVar, Constraint)    
processTerm ctx stx q constr tr = 
    let (tyT, q', constr')  = recon ctx stx q tr    in 
    let tr'                 = eval  ctx stx tr      in 
    let constr''            = constr ++ constr' in 
    let sol                 = unify ctx constr'' in 
    let tyT'                = double (apply_constr sol) tyT in 
    (tr', tyT', ctx, q', constr'') 



typingTest cn = processCN [] [] 0 [] cn  
