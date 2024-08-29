-- Program Graph 


module PG where 

import GCLL 
import Node 
import Edge
import Type
import Term
import Tree
import AST
import Hex
import Action  
import Data
import Datatype

import Data.Tuple.Extra (fst3, snd3, thd3) 

--------------------------
--- DATA DEFINITION    ---
--------------------------


-- || PROGRAM GRAPH || --
-- type Edge = (Node, Action, Node) 


-- CONFIGURE 
type FreshNode  = Int
type FreshSto   = Int 
type FreshVar   = Int
type INode      = Node Int
type TNode      = Node Int 
type ArgNum     = Int
{--
data Bind       = FunBind (ArgNum, INode, TNode) 
                | VarBind 
                deriving (Show, Eq, Read) 
                --}

{-- Function Context consits of 3 components 
 -  1. The number of Args
 -  2. Initial Node of Program Graph of the function 
 -  3. Terminal Node of Program Graph of the function --} 
type FunCtx     = [Maybe (ArgNum, INode, TNode)] 

{-- Function Execution (or Function Call ) 
 - 1. push Arguments and PUSH "Record A"  
 - 2. goto Function Init Node 
 - 3. Function Execution
 - 4. if the top element of the Record/check stack is "Record A" then exit Function Terminal Node. This process is named "CHECK A".
 - 5. exit. --} 


-- Variables are contained in the reverse order . 
-- i.e. 
-- given a function (F X Y = term) , de Bruijn Var is as follows; 
-- F : VAR 2 
-- X : VAR 1 
-- Y : VAR 0 
-- the variables are ordered by the VAR n, 
-- so [Y,X,F] 

paramDegrees :: [(ID,Ty)] -> [(ID,ArgLen)] 
paramDegrees []                     = [] 
paramDegrees ((id,ty):xs)           = (id,degreeOfFun ty):paramDegrees xs

degreeOfFun :: Ty -> Int
degreeOfFun (TyABS tyA tyB)         = degreeOfFun tyB + 1 
degreeOfFun _                       = 0  

type ArgLen = Int 

pgFunCtxs :: [(ID,ArgLen)] -> Config -> Config 
pgFunCtxs []     cfg   =   cfg 
pgFunCtxs (f:fs) cfg   =   pgFunCtxs fs (pgFunCtx f cfg)  

pgFunCtx :: (ID,ArgLen) -> Config -> Config 
pgFunCtx (id,0)      (i,t,q,s,v,ctx)   =  (i,t,q,s,v, Nothing : ctx) 
pgFunCtx (id,arglen) (i,t,q,s,v,ctx)   =  (qn,qx,q+2,s,v,Just(arglen,qn,qx) : ctx)   
                where (qn,qx) = (Q q, Q(q+1)) 

searchFun :: Int -> FunCtx -> Maybe (ArgNum,INode,TNode) 
searchFun i []      = Nothing 
searchFun 0 (x:xs)  = x  
searchFun n (x:xs)  = searchFun (n-1) xs 

rmctx n []     = error "contex cannot be removed" 
rmctx 0 ctx    = ctx 
rmctx n (c:cs) = rmctx (n-1) cs  


    




type Config     = (INode, TNode, FreshNode, FreshSto, FreshVar, FunCtx) 

-- PROGRAM GRAPH with Configure 
type Edges  = ([Edge(Node Int)Action], Config) 


-- data PG = PG [Nd] Edges Nd [Nd]  


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
pgTOP (SV id ty)        (i,t,q,s,v,ctx) =   ([(i, AcSto s, Q q)], (Q q, t, q+1,s+1,v,ctx))   
pgTOP (EV id ty)        (i,t,q,s,v,ctx) =   undefined 
pgTOP (DT id ids cnstrs)(i,t,q,s,v,ctx) =   undefined 

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




-- Decl -> pg  
pgDecl :: Decl -> Config -> Edges 
pgDecl d cfg@(i,t,q,s,v,ctx)    = case d of 
    LET  id    tm fm        -> (es,cfg'')  where 
        cfg'                                = pgFunCtxs [(id,0)] cfg 
        (es,cfg'')                          = pgTerm tm cfg' 
-- here, tm cannot always be AExp a 
-- so this cannot be translated into " x := a "  
-- it should be like 
-- pg(tm ; x := retvalue) ; 
    SLET id    tm _         ->  ([]     ,   (i,t,q  ,s  ,v  ,ctx   ))
    FLET id ps tm _         ->  (es,   (i,t,q''',s''',v''',ctx'))  where 
        arglen                              = length        ps                    
        params                              = paramDegrees  ps       
        (i',t',q',s',v',ctx')               = pgFunCtxs [(id,arglen)] (i,t,q,s,v,ctx)       
        Just (_,qn,qx)                      = searchFun 0 ctx'
        (_ ,_ ,q'',s'',v'',ctx'')           = pgFunCtxs params  (i',t',q',s',v',ctx')       
        (es,(_,_,q''',s''',v''',ctx'''))    = pgTerm tm   (qn,qx,q'',s'',v'',ctx'') 
    --DATA id is ds           ->  undefined 


pgTerm :: Term -> Config -> Edges 
pgTerm tr cfg@(i,t,q,s,v,ctx)       = case tr of 
    RED TmAPP [t1,t2]           ->  pgTermApp tr cfg []
    RED (TmU256 n) []           ->  ([(i, AcPush (Ox (toHex n           )), t)], cfg) 
    RED (TmDATA d) []           ->  ([(i, AcPush (Ox (toHex (data2nat d))), t)], cfg)
    RED (TmVAR  n) []           ->  ([(i, AcPush (Var (show n           )), t)], cfg) 
    RED TmIF [b,t1,t2]          ->  (eb++e1++e2++eelse, cfg')  where 
        (eb,(_,_,q' ,s' ,v' ,ctx' ))    = pgCond b  (i, Q q,q+2,s  ,v  ,ctx  ) 
        (e1,(_,_,q'',s'',v'',ctx''))    = pgTerm t1 (Q q ,t,q' ,s' ,v' ,ctx' ) 
        (e2,cfg')                       = pgTerm t2 (Q(q+1),t,q'',s'',v'',ctx'') 
        eelse                           = [(i, AcSkip, Q(q+1))]    
    RED(TmBOP o)[t1,t2]         ->  (e1++e2++[(Q q',AcBop o,t)],cfg)  where 
        (e1,(_,_,q',s',v',ctx'))        = pgTerm t1 (i,Q q,q+1,s,v,ctx) 
        (e2,cfg)                        = pgTerm t2 (Q q,Q q',q'+1,s',v',ctx') 
    _                           ->  ([], cfg) 

pgTermApp :: Term -> Config -> [Term] -> Edges 
pgTermApp (RED TmAPP [t1,t2]) cfg@(i,t,q,s,v,ctx) cont = case t1 of 
    RED (TmVAR n) []    -> (eent++econt++ercd++echk++eexit, (i,t,q'+1,s',v',ctx'))  where 
        Just (argnum,qn,qx)         = searchFun n ctx 
        argnums                     = map (fst3 <$>) ctx 
        (econt,(_,_,q',s',v',ctx')) = pgArgs(t2:cont)(Q q,Q(q+1),q+2,s,v,ctx) argnums 0
        eent                        = [(i     , AcEnter     , Q q     )] 
        ercd                        = [(Q(q+1), AcRecord i t, qn      )] 
        echk                        = [(qx    , AcCheck  i t, Q q'    )]  
        eexit                       = [(Q q'  , AcExit      , t       )] 
    _                   -> pgTermApp t1 cfg (t2:cont) 

pgArgs :: [Term] -> Config -> [Maybe Int] -> Int -> Edges 
pgArgs []                   cfg             ns           k = ([],cfg) 
pgArgs [tm]                 (i,t,q,s,v,ctx) (Nothing:ns) k = pgTerm tm (i,t,q,s,v,ctx) 
pgArgs (tm:tms)             (i,t,q,s,v,ctx) (Nothing:ns) k = (e ++ econt, (i,t,q'',s'',v'',ctx'')) where 
    (e    ,(_,_,q' ,s' ,v' ,ctx' )) = pgTerm tm  (i,Q q,q+1,s,v,ctx)             
    (econt,(_,_,q'',s'',v'',ctx'')) = pgArgs tms (Q q,t,q',s',v',ctx') ns (k+1) 
--pgArgs (RED(TmVAR j)[]:tms) (i,t,q,s,v,ctx) (n:ns) k = pgArgs tms (i,t,q,s,v,ctx) ns (k+1)  
    -- Higher Order Variable is Skipped since We do not push values to the stack before function call.
    -- Rather, we connect edges. 
-- possibility 
-- 1. in function defintion,  variable call
-- 2. in function defintion,  function 
-- 3. in function call     ,  defined function variable 
-- 4. in function call     ,  just value 
pgArgs [tm]             (i,t,q,s,v,ctx) (Just n:ns)       k = (e, (i,t,q',s',v',ctx')) where 
    (e    ,(_,_,q' ,s' ,v' ,ctx' )) = pgTerm tm  (i,t,q,s ,v ,ctx ) 
pgArgs (tm:tms)             (i,t,q,s,v,ctx) (Just n:ns)       k = (e++econt, (i,t,q'',s'',v'',ctx'')) where 
    (e    ,(_,_,q' ,s' ,v' ,ctx' )) = pgTerm tm  (i,Q q,q+1 ,s ,v ,ctx ) 
    (econt,(_,_,q'',s'',v'',ctx'')) = pgArgs tms (Q q, t, q',s',v',ctx') ns (k+1) 
pgArgs (tm:tms)             (i,t,q,s,v,ctx) (n:ns)       k = (e++econt, (i,t,q'',s'',v'',ctx'')) where 
    Just (_,qf,qF)                  = searchFun k ctx 
    (e    ,(_,_,q' ,s' ,v' ,ctx' )) = pgTerm tm  (qf,qF,q ,s ,v ,ctx ) 
    (econt,(_,_,q'',s'',v'',ctx'')) = pgArgs tms (i, t, q',s',v',ctx') ns (k+1) 


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

    
