-- Program Graph 


module PG where 

import GCLL 
import Type
import Term
import Tree
import AST
import Hex
import Action  

import Data.Tuple.Extra (fst3) 

--------------------------
--- DATA DEFINITION    ---
--------------------------

-- PROGRAM GRAPH 
type Edge = (Node, Action, Node) 

-- CONFIGURE 
type NewNode    = Int
type NewSto     = Int 
type NewVar     = Int
type InitNode   = Node 
type LastNode   = Node
type ArgNum     = Int
data Bind   = FunBind (ArgNum, InitNode, LastNode) 
            | VarBind 
            deriving (Show, Eq, Read) 
type FunCtx = [(ArgNum, InitNode, LastNode)] 
type Config = (InitNode, LastNode, NewNode, NewSto, NewVar, FunCtx) 

type Edges  = ([Edge], Config) 


-- Initialization 
initialConfig = (Qi, Qt, 1, 0, 0, [])
mkPG cn = pgCN cn initialConfig  






-------------------------
--    Program Graph    --  
-------------------------

-- CN -> pg 
pgCN :: CONTRACT -> Config -> Edges
pgCN (CN id tops) config = pgTOPs tops config


-- [TOP] -> pg 
pgTOPs :: [TOP] -> Config -> Edges 
pgTOPs []           cfg             =   ([], cfg)
pgTOPs (top:tops)   cfg             =   let (es, cfg' ) = pgTOP  top  cfg in 
                                        let (ess,cfg'') = pgTOPs tops cfg' in 
                                        (es ++ ess, cfg'') 
    
-- TOP -> pg     
pgTOP :: TOP -> Config -> Edges
pgTOP (MT id ty ps bd) (i,t,q,s,v,ctx) = 
    let (e,(_,_,q',s',v',ctx')) = pgMT (MT id ty ps bd) (Q q,Q(q+1),q+2,s,v,ctx) in 
    (e++[(i,AcDispatch id, Q q),(Q(q+1),AcSkip, t)], (i,t,q',s',v',ctx')) 
pgTOP (SV id ty) (i,t,q,s,v,ctx)    =   ([(i, AcSto s, Q q)], (Q q, t, q+1,s+1,v,ctx))   
pgTOP (EV id ty) (i,t,q,s,v,ctx)    =   undefined 



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
pgDecls [] cnf = ([],cnf)
pgDecls (FLET id ps tm fm:ds) (i,t,q,s,v,ctx) = (es++ess,cnf'') 
                                where   (es, (i',t',q',s',v',ctx')) = pgDecl (FLET id ps tm fm) (i,t,q,s,v,ctx) 
                                        (ess, cnf'')                = pgDecls ds (i,t,q',s',v',ctx') 
pgDecls (d:ds) (i,t,q,s,v,ctx) =   (es++ess, cnf'') 
                                where   (es, (i',t',q',s',v',ctx')) = pgDecl  d  (i,Q q,q+1,s,v,ctx) 
                                        (ess, cnf'')              = pgDecls ds (Q q,t,q',s',v',ctx') 

higherParams []             = [] 
higherParams ((id,ty):xs)   = (id,degreeOfFun ty):higherParams xs


degreeOfFun :: Ty -> Int
degreeOfFun (TyABS tyA tyB) = max (degreeOfFun tyA) (degreeOfFun tyB) + 1 
degreeOfFun _               = 0  

type ArgLen = Int 

pgFuns :: [(ID,ArgLen)] -> Config -> Edges
pgFuns [] cfg       =   ([],cfg) 
pgFuns (f:fs) cfg   =   (es++ess,cfg'') 
                where   (es,cfg')   = pgFun  f  cfg 
                        (ess,cfg'') = pgFuns fs cfg'

pgFun :: (ID,ArgLen) -> Config -> Edges 
pgFun (id,arglen) (i,t,q,s,v,ctx) = 
                let (qn,qx) = (Q  q   , Q (q+1)) in 
                ([], (qn,qx,q+2,s,v,(arglen,qn,qx):ctx))   


removectx n []     = error "contex cannot be removed" 
removectx 0 ctx    = ctx 
removectx n (c:cs) = removectx (n-1) cs  

-- Decl -> pg  
pgDecl :: Decl -> Config -> Edges 
pgDecl (FLET id ps tm _  ) (i,t,q,s,v,ctx) = 
    let arglen      = length ps                     in 
    let fparams     = higherParams ps               in 
    let funs        = (id, arglen) : fparams        in  
    let (es,(_,_,q',s',v',ctx'))            = pgFuns funs (i,t,q,s,v,ctx)       in 
    let (_,qn,qx)   = case searchFun 1 ctx' of 
                            Just x -> x 
                            Nothing -> error $ show ctx' ++ "function not found"  in 
    let (es',(_,_,q'',s'',v'',ctx''))       = pgTerm tm   (qn,qx,q',s',v',ctx') in 
    let ctx''' = removectx (arglen - length fparams) ctx'' in 
    (es++es',(i,t,q'',s'',v'',ctx''')) 
pgDecl ( LET id    tm fm ) (i,t,q,s,v,ctx)  = pgTerm tm (i,t,q,s,v,ctx) 
pgDecl (SLET id    tm _  ) (i,t,q,s,v,ctx)  = ([], (i,t,q,s,v,ctx) )
-- here, tm cannot always be AExp a 
-- so this cannot be translated into " x := a "  
-- it should be like 
-- pg(tm ; x := retvalue) ; 
pgDecl (DATA id is ds    ) (i,t,q,s,v,ctx)  = undefined 


pgArgs :: [Term] -> Config -> [Int] -> Int -> Edges 
pgArgs []       cfg             ns                 k = ([],cfg) 
pgArgs [tm]                 (i,t,q,s,v,ctx) (0:ns) k = pgTerm tm (i,t,q,s,v,ctx) 
pgArgs (tm:tms)             (i,t,q,s,v,ctx) (0:ns) k = 
    let (e,(_,_,q',s',v',ctx'))         = pgTerm tm  (i,Q q,q+1,s,v,ctx)            in 
    let (econt,(_,_,q'',s'',v'',ctx'')) = pgArgs tms (Q q,t,q',s',v',ctx') ns (k+1) in 
    (e ++ econt, (i,t,q'',s'',v'',ctx'')) 
pgArgs (RED(TmVAR j)[]:tms) (i,t,q,s,v,ctx) (n:ns) k = pgArgs tms (i,t,q,s,v,ctx) ns (k+1)  
pgArgs (tm:tms)             (i,t,q,s,v,ctx) (n:ns) k = 
    let (_,qf,qF) = case searchFun k ctx of 
                        Just x  -> x 
                        Nothing -> error "searchFun: not found" in 
    let (e,(_,_,q',s',v',ctx'))         = pgTerm tm (qf,qF,q,s,v,ctx) in 
    let (econt,(_,_,q'',s'',v'',ctx'')) = pgArgs tms (i,t,q',s',v',ctx') ns (k+1) in 
    (e ++ econt, (i,t,q'',s'',v'',ctx''))


--
--pgReturnTerm :: Term -> Config -> Edges 
--pgReturnTerm tm (i,t,q,s,v,ctx)     = 
--    let (e,ctx')                    = pgTerm tm (i,t,q,s,v,ctx) in 
--    undefined 


pgTermApp :: Term -> Config -> [Term] -> Edges 
pgTermApp (RED TmAPP [RED(TmVAR n)[],t2]) (i,t,q,s,v,ctx) cont = 
    let Just (argnum,qn,qx)         = searchFun n ctx in 
    let eenter                      = [(i     , AcEnter     , Q q     )] in 
    let argnums                     = map fst3 ctx in 
    let (econt,(_,_,q',s',v',ctx')) = pgArgs(t2:cont)(Q q,Q(q+1),q+2,s,v,ctx)argnums 1 in
    let erecord                     = [(Q(q+1), AcRecord i t, qn      )] in 
    let echeck                      = [(qx    , AcCheck  i t, Q q'    )] in 
    let eexit                       = [(Q q'  , AcExit      , t)] in 
    (eenter ++ econt ++ erecord ++ echeck ++ eexit, (i,t,q'+1,s',v',ctx'))  
pgTermApp (RED TmAPP [t1,t2]) cfg cont  = pgTermApp t1 cfg (t2:cont) 


pgTerm :: Term -> Config -> Edges 
pgTerm (RED TmAPP [t1,t2]) cfg              = pgTermApp (RED TmAPP [t1,t2]) cfg []
pgTerm (RED TmIF [b,t1,t2]) (i,t,q,s,v,ctx) = 
    let (eb,(_,_,q' ,s' ,v' ,ctx' ))    = pgCond b  (i, Q q,q+2  ,s  ,v  ,ctx  ) in 
    let (e1,(_,_,q'',s'',v'',ctx''))    = pgTerm t1 (Q q   ,t,q' ,s' ,v' ,ctx' ) in 
    let (e2,cfg)                        = pgTerm t2 (Q(q+1),t,q'',s'',v'',ctx'') in 
    let eelse                           = [(i, AcSkip, Q(q+1))] in 
    (eb++e1++e2++eelse, cfg)  
pgTerm (RED (TmU256 n) []) (i,t,q,s,v,ctx)  = ([(i, AcPush (Ox (toHex n)), t)], (i,t,q,s,v,ctx)) 
pgTerm (RED (TmVAR  n) []) (i,t,q,s,v,ctx)  = ([(i, AcPush (Var (show n)), t)], (i,t,q,s,v,ctx))
pgTerm (RED (TmBOP o) [t1,t2]) (i,t,q,s,v,ctx) = 
    let (e1,(_,_,q',s',v',ctx'))        = pgTerm t1 (i,Q q,q+1,s,v,ctx) in 
    let (e2,cfg)                        = pgTerm t2 (Q q,Q q',q'+1,s',v',ctx') in 
    (e1++e2++[(Q q',AcBop o,t)],cfg)  
pgTerm _ cfg = 
    ([], cfg) 


searchFun :: Int -> FunCtx -> Maybe (ArgNum,Node,Node) 
searchFun i []      = Nothing 
searchFun 0 (x:xs)  = Just x  
searchFun n (x:xs)  = searchFun (n-1) xs 

pgCond :: Term -> Config -> Edges 
pgCond (RED (TmBOP op) [t1,t2]) (i,t,q,s,v,ctx) = 
    ([(i,AcBool ((pgBOP op) t1' t2'), t)], (i,t,q,s,v,ctx)) 
    where 
        t1' = tm2exp ctx t1
        t2' = tm2exp ctx t2 

pgBOP "!=" x y = Not (Eq x y)  
pgBOP "==" x y = Eq  x y
pgBOP "*"  x y = Mul x y
pgBOP "-"  x y = Sub x y
pgBOP "+"  x y = Add x y
pgBOP "<"  x y = Lt  x y
pgBOP ">"  x y = Gt  x y 

tm2exp ctx (RED (TmVAR n) [])  = Var (show n)  
tm2exp ctx (RED (TmU256 n) []) = Ox (toHex n)

--
--pgTerm (RED TmIF [b,t1,t2]) cfg = 
--    let (es,cfg')   = pgTerm b cfg in 
--    let (es',cfg'') = pgTerm t1 cfg' in 
--    let (es'',cfg''')   = pgTerm t2 cfg'' in 
--    (es++es'++es'', cfg'') 
--pgTerm (RED (TmBOP "==") [t1,t2]) (i,t,q,s,v,ctx) = 
--    let (e1,(i',t',q',s',v',ctx'))   = pgTerm t1 (i,Q q,q+1,s,v,ctx) in 
--    let (e2,(i'',t'',q'',s'',v'',ctx'')) = pgTerm t2 (Q q,Q(q'+1),q'+1,s',v',ctx') in 
    
