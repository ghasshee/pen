module Eval where 


import AST
import Term
import Type
import Tree
import Bind 
import Typing 






eval :: Ctx -> Term -> Term 
eval ctx tr = tr 




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



processCN :: Ctx -> UVar -> Constraint -> CONTRACT -> (CONTRACT, Ctx, UVar, Constraint) 
processCN ctx q constr (CN id tops)     = (CN id tops', ctx', q', constr') where 
    (tops', ctx', q', constr')           = processTOPs ctx q constr tops

processTOPs :: Ctx -> UVar -> Constraint -> [TOP] -> ([TOP], Ctx, UVar, Constraint) 
processTOPs ctx q constr tops = case tops of 
    []                                  -> ([], ctx, q, constr) 
    MT id ty ps (BODY p1 ds t p2) : ts   -> (m' : ts', ctx''', q''', constr''') where
        (ds',ctx',q',constr')           = processDecls ctx  q  constr ds 
        (t',rety, ctx'', q'', constr'') = processTerm  ctx' q' constr' t
        m'                              = MT id ty' ps (BODY p1 ds' t' p2)   
        ty'                             = paramret2ty ps rety 
        (ts', ctx''', q''', constr''')  = processTOPs ctx'' q'' constr'' ts
        

paramret2ty :: [Param] -> Ty -> Ty 
paramret2ty ps rety = loop (reverse ps) rety where 
    loop []             ty = ty 
    loop ((_,t1):rest)  ty = TyARR t1 ty 

addParamBind ctx params q = case params of 
    []                      -> (q, ctx) 
    (p,Untyped) : rest      -> (q', ctx'') where 
        tyP             = TyID (var q) 
        ctx'            = addBind ctx p (BindTmVAR tyP)
        (q', ctx'')     = addParamBind ctx' rest (q+1)   
    (p,tyP) : rest          -> (q', ctx'') where 
        ctx'            = addBind ctx p (BindTmVAR tyP)
        (q', ctx'')     = addParamBind ctx' rest q 

processDecls :: Ctx -> UVar -> Constraint -> [Decl] -> ([Decl], Ctx, UVar, Constraint) 
processDecls ctx q constr ds = case ds of 
    []                      -> ([], ctx, q, constr) 
    FLET id ps _ tr f : ds  -> (d':ds', ctx''', q'', constr'') where 
        tyF                         = TyID (var q) 
        _ctx                        = addBind ctx id (BindTmVAR tyF) 
        (_q,__ctx)                  = addParamBind _ctx ps (q+1) 
        (t, ty, ctx', q', constr')  = processTerm __ctx _q constr tr 
        ctx''                       = addBind ctx' id (BindTmVAR ty)  
        (ds', ctx''', q'', constr'')= processDecls ctx'' q' constr' ds
        d'                          = FLET id ps (paramret2ty ps ty) t f  
    LET  id    _ tr f : ds  -> (d':ds', ctx''', q'', constr'') where 
        (t, ty, ctx', q', constr')  = processTerm ctx q constr tr 
        ctx''                       = addBind ctx' id (BindTmVAR ty) 
        (ds',ctx''', q'', constr'') = processDecls ctx'' q' constr' ds
        d'                          = LET id     ty t f 
    SLET id    _ tr f : ds  -> (d':ds', ctx''', q'', constr'') where  
        (t, ty, ctx', q', constr')  = processTerm ctx q constr tr 
        ctx''                       = addBind ctx' id (BindTmVAR ty) 
        (ds',ctx''', q'', constr'') = processDecls ctx'' q' constr' ds
        d'                          = SLET id ty t f 


processTerm :: Ctx -> UVar -> Constraint -> Term -> (Term, Ty, Ctx, UVar, Constraint)    
processTerm ctx q constr tr = 
    let (tyT, q', constr')  = recon ctx q tr    in 
    let tr'                 = eval  ctx tr      in 
    let constr''            = constr ++ constr' in 
    let sol                 = unify ctx constr' in 
    let tyT'                = double (apply_constr sol) tyT in 
    (tr', tyT', ctx, q', constr'') 



typingTest cn = processCN [] 0 [] cn  
