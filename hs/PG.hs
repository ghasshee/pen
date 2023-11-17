-- Program Graph 


module PG where 

import GCLL 
import Node 
import Type
import Term
import Tree
import AST
import Hex
import Action  
import Data

import Data.Tuple.Extra (fst3, snd3, thd3) 

--------------------------
--- DATA DEFINITION    ---
--------------------------


-- || PROGRAM GRAPH || --
-- type Edge = (Node, Action, Node) 


-- CONFIGURE 
type NewNode    = Int
type NewSto     = Int 
type NewVar     = Int
type InitNode   = Nd
type LastNode   = Nd
type ArgNum     = Int
data Bind       = FunBind (ArgNum, InitNode, LastNode) 
                | VarBind 
                deriving (Show, Eq, Read) 
type FunCtx     = [(ArgNum, InitNode, LastNode)] 
type Config     = (InitNode, LastNode, NewNode, NewSto, NewVar, FunCtx) 

type Edges  = ([Edge Int Action], Config) 


data PG = PG [Nd] Edges Nd [Nd]  


-- Initial Configuration  
-- initialConfig = (Qi, Qt, 1, 0, 0, [])
initialConfig = (Q 1,Q 2, 3, 0, 0, []) 


mkPG :: CONTRACT -> Edges
mkPG cn = pgCN cn initialConfig  






-------------------------
--    Program Graph    --  
-------------------------

-- CN -> pg 
pgCN    :: CONTRACT -> Config -> Edges
pgCN (CN id tops) config    = pgTOPs tops config

-- [TOP] -> pg 
pgTOPs  :: [TOP] -> Config -> Edges 
pgTOPs  []           cfg    =   ([], cfg)
pgTOPs  (top:tops)   cfg    =   (es ++ ess, cfg'') where 
                                    (es, cfg' ) = pgTOP  top  cfg 
                                    (ess,cfg'') = pgTOPs tops cfg'  
                                                            

-- TOP -> pg     
pgTOP :: TOP -> Config -> Edges
pgTOP (MT id ty ps bd) (i,t,q,s,v,ctx) = 
    let (e,(_,_,q',s',v',ctx')) = pgMT (MT id ty ps bd) (Q q,Q(q+1),q+2,s,v,ctx) in 
    (e++[(i,AcDispatch id, Q q),(Q(q+1),AcSkip, t)], (i,t,q',s',v',ctx')) 
pgTOP (SV id ty) (i,t,q,s,v,ctx)    =   ([(i, AcSto s, Q q)], (Q q, t, q+1,s+1,v,ctx))   
pgTOP (EV id ty) (i,t,q,s,v,ctx)    =   undefined 

pgMT :: TOP -> Config -> Edges
pgMT (MT id ty ps bd) cfg           =   let (es ,cfg' )     = pgParams ps cfg   in 
                                        let (es',cfg'')     = pgBODY   bd cfg'  in 
                                        (es++es', cfg'') 

pgParams :: [Param] -> Config -> Edges
pgParams []     cfg                 =   ([], cfg)
pgParams (p:ps) cfg                 =   (es ++ ess, cfg'') 
                                where   (es, cfg' ) = pgParam  p  cfg  
                                        (ess,cfg'') = pgParams ps cfg' 

pgParam :: Param -> Config -> Edges
pgParam (id,ty) (i,t,q,s,v,ctx)     =   ([(i, AcVar v, Q q)], (Q q, t, q+1,s,v+1,ctx))

-- BODY -> pg 
pgBODY :: BODY -> Config -> Edges 
pgBODY (BODY _ ds tm _) cnf         =   (es++es', cnf'') 
                                where   (es,  cnf' ) = pgDecls ds cnf  
                                        (es', cnf'') = pgTerm  tm cnf' 

pgDecls :: [Decl] -> Config -> Edges 
pgDecls []     cfg                  = ([],cfg)
pgDecls (d:ds) (i,t,q,s,v,ctx)      = case d of 
    FLET id ps tm fm                ->  (es++ess,cfg'')
                                where   (es, (i',t',q',s',v',ctx')) = pgDecl  d  (i  ,t  ,q  ,s ,v ,ctx ) 
                                        (ess, cfg'')                = pgDecls ds (i  ,t  ,q' ,s',v',ctx')
    _                               ->  (es++ess, cfg'') 
                                where   (es, (i',t',q',s',v',ctx')) = pgDecl  d  (i  ,Q q,q+1,s ,v ,ctx ) 
                                        (ess, cfg'')                = pgDecls ds (Q q,t  ,q' ,s',v',ctx') 

higherParams []                     = [] 
higherParams ((id,ty):xs)           = (id,degreeOfFun ty):higherParams xs

degreeOfFun :: Ty -> Int
degreeOfFun (TyABS tyA tyB)         = max (degreeOfFun tyA) (degreeOfFun tyB) + 1 
degreeOfFun _                       = 0  

type ArgLen = Int 

pgFuns :: [(ID,ArgLen)] -> Config -> Edges
pgFuns []     cfg   =   ([],cfg) 
pgFuns (f:fs) cfg   =   (es++ess,cfg'') 
                where   (es ,cfg' ) = pgFun  f  cfg 
                        (ess,cfg'') = pgFuns fs cfg'

pgFun :: (ID,ArgLen) -> Config -> Edges 
pgFun (id,arglen) (i,t,q,s,v,ctx)   =  ([], (qn,qx,q+2,s,v,(arglen,qn,qx):ctx))   
                where (qn,qx) = (Q  q   , Q (q+1)) 


rmctx n []     = error "contex cannot be removed" 
rmctx 0 ctx    = ctx 
rmctx n (c:cs) = rmctx (n-1) cs  

-- Decl -> pg  
pgDecl :: Decl -> Config -> Edges 
pgDecl d (i,t,q,s,v,ctx)    = case d of 
    DATA id is ds           ->  undefined 
    LET  id    tm fm        ->  pgTerm tm (i,t,q,s,v,ctx) 
-- here, tm cannot always be AExp a 
-- so this cannot be translated into " x := a "  
-- it should be like 
-- pg(tm ; x := retvalue) ; 
    SLET id    tm _         ->  ([]     ,   (i,t,q  ,s  ,v  ,ctx   ))
    FLET id ps tm _         ->  (es++es',   (i,t,q'',s'',v'',ctx'''))  where 
        arglen                          = length        ps                    
        fparams                         = higherParams  ps       
        (es,(_,_,q',s',v',ctx'))        = pgFuns ((id,arglen):fparams) (i,t,q,s,v,ctx)       
        Just (_,qn,qx)                  = searchFun 1 ctx' 
        (es',(_,_,q'',s'',v'',ctx''))   = pgTerm tm   (qn,qx,q',s',v',ctx') 
        ctx'''                          = rmctx (arglen - length fparams) ctx'' 


pgArgs :: [Term] -> Config -> [Int] -> Int -> Edges 
pgArgs []       cfg             ns                 k = ([],cfg) 
pgArgs [tm]                 (i,t,q,s,v,ctx) (0:ns) k = pgTerm tm (i,t,q,s,v,ctx) 
pgArgs (tm:tms)             (i,t,q,s,v,ctx) (0:ns) k = (e ++ econt, (i,t,q'',s'',v'',ctx'')) where 
    (e    ,(_,_,q' ,s' ,v' ,ctx' )) = pgTerm tm  (i,Q q,q+1,s,v,ctx)             
    (econt,(_,_,q'',s'',v'',ctx'')) = pgArgs tms (Q q,t,q',s',v',ctx') ns (k+1) 
pgArgs (RED(TmVAR j)[]:tms) (i,t,q,s,v,ctx) (n:ns) k = pgArgs tms (i,t,q,s,v,ctx) ns (k+1)  
pgArgs (tm            :tms) (i,t,q,s,v,ctx) (n:ns) k = (e++econt, (i,t,q'',s'',v'',ctx'')) where 
    Just (_,qf,qF)                  = searchFun k ctx 
    (e    ,(_,_,q' ,s' ,v' ,ctx' )) = pgTerm tm  (qf,qF,q ,s ,v ,ctx ) 
    (econt,(_,_,q'',s'',v'',ctx'')) = pgArgs tms (i, t, q',s',v',ctx') ns (k+1) 


pgTermApp :: Term -> Config -> [Term] -> Edges 
pgTermApp (RED TmAPP [t1,t2] ) cfg@(i,t,q,s,v,ctx) cont = case t1 of 
    RED (TmVAR n) []    -> (eent++econt++ercd++echk++eexit, (i,t,q'+1,s',v',ctx'))  where 
        Just (argnum,qn,qx)         = searchFun n ctx 
        argnums                     = map fst3 ctx 
        (econt,(_,_,q',s',v',ctx')) = pgArgs(t2:cont)(Q q,Q(q+1),q+2,s,v,ctx)argnums 1 
        eent                        = [(i     , AcEnter     , Q q     )] 
        ercd                        = [(Q(q+1), AcRecord i t, qn      )] 
        echk                        = [(qx    , AcCheck  i t, Q q'    )]  
        eexit                       = [(Q q'  , AcExit      , t       )] 
    _                   -> pgTermApp t1 cfg (t2:cont) 


pgTerm :: Term -> Config -> Edges 
pgTerm tr cfg@(i,t,q,s,v,ctx)       = case tr of 
    RED (TmU256 n) []           ->  ([(i, AcPush (Ox (toHex n           )), t)], cfg) 
    RED (TmDATA d) []           ->  ([(i, AcPush (Ox (toHex (data2nat d))), t)], cfg)
    RED (TmVAR  n) []           ->  ([(i, AcPush (Var (show n           )), t)], cfg) 
    RED TmAPP [t1,t2]           ->  pgTermApp (RED TmAPP [t1,t2]) cfg []
    RED TmIF [b,t1,t2]          ->  (eb++e1++e2++eelse, cfg')  where 
        (eb,(_,_,q' ,s' ,v' ,ctx' ))    = pgCond b  (i, Q q,q+2  ,s  ,v  ,ctx  ) 
        (e1,(_,_,q'',s'',v'',ctx''))    = pgTerm t1 (Q q   ,t,q' ,s' ,v' ,ctx' ) 
        (e2,cfg')                       = pgTerm t2 (Q(q+1),t,q'',s'',v'',ctx'') 
        eelse                           = [(i, AcSkip, Q(q+1))]    
    RED(TmBOP o)[t1,t2]         ->  (e1++e2++[(Q q',AcBop o,t)],cfg)  where 
        (e1,(_,_,q',s',v',ctx'))        = pgTerm t1 (i,Q q,q+1,s,v,ctx) 
        (e2,cfg)                        = pgTerm t2 (Q q,Q q',q'+1,s',v',ctx') 
    _                           ->  ([], cfg) 


searchFun :: Int -> FunCtx -> Maybe (ArgNum,Nd,Nd) 
searchFun i []      = Nothing 
searchFun 0 (x:xs)  = Just x  
searchFun n (x:xs)  = searchFun (n-1) xs 

pgCond :: Term -> Config -> Edges 
pgCond (RED (TmBOP op) [t1,t2]) (i,t,q,s,v,ctx) = 
    ([(i,AcBool ((pgBOP op) t1' t2'), t)], (i,t,q,s,v,ctx)) where 
        t1' = tm2exp ctx t1
        t2' = tm2exp ctx t2 

pgBOP "!=" x y = Not (Eq x y)  
pgBOP "==" x y = Eq  x y
pgBOP "*"  x y = Mul x y
pgBOP "-"  x y = Sub x y
pgBOP "+"  x y = Add x y
pgBOP "<"  x y = Lt  x y
pgBOP ">"  x y = Gt  x y 


tm2exp :: a -> Term -> EXPR 
tm2exp ctx (RED (TmVAR  n) [])  =   Var (show n)  
tm2exp ctx (RED (TmSTO  n) [])  =   GCLL.S   (Ox (toHex $ toInteger n))
tm2exp ctx (RED (TmU256 n) [])  =   Ox (toHex n)
tm2exp ctx (RED (TmDATA d) [])  =   Ox (toHex (data2nat d))

    
