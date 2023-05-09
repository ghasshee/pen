-- Program Graph 


module PG where 

import GCLL 
import Type
import Term
import Tree
import AST
import Hex



data Var  = X Int deriving (Show, Eq, Read) 
data Sto  = S Int deriving (Show, Eq, Read) 
data Node = Q Int 
          | Qi | Qt | Qe
                deriving (Show, Eq, Read) 

data Action     = AcStop 
                | AcRevert EXPR EXPR
                | AcReturn EXPR EXPR
                | AcPop 
                | AcPush EXPR
                | AcSwap Int 
                | AcDup  Int 
                | AcBop  String 
                | AcCalldatacopy EXPR EXPR EXPR 
                | AcCodecopy EXPR EXPR EXPR
                | AcExtcodecopy 
                -- | AcSeq [Action] 
                | AcSkip                -- correspond to GOTO 
                | AcAssgin Var EXPR
                | AcBool   EXPR         -- correspond to IFGOTO
                | AcElse                -- Does this  
                | AcEnter               -- make empty frame 
                | AcExit                -- Return stacktop, remove frame, push stacktop
                | AcVar Int
                | AcSto Int
                | AcArray Var Int
                | AcRecord Node Node    -- 
                | AcCheck Node Node 
                deriving (Show, Eq, Read) 
                


type Edge = (Node, Action, Node) 
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

initialConfig = (Qi, Qt, 0, 0, 0, [])


mkPG cn = pgCN cn initialConfig  

pgCN :: CONTRACT -> Config -> Edges
pgCN (CN id tops) config = pgTOPs tops config

pgTOPs :: [TOP] -> Config -> Edges 
pgTOPs []           cfg     =   ([], cfg)
pgTOPs (top:tops)   cfg     =   let (es, cfg' ) = pgTOP  top  cfg in 
                                let (ess,cfg'') = pgTOPs tops cfg' in 
                                (es ++ ess, cfg'') 
    
pgTOP :: TOP -> Config -> Edges
pgTOP (MT id ty ps bd) cfg          =   let (es,cfg') = pgParams ps cfg in 
                                        let (es',cfg'') = pgBODY bd cfg' in 
                                        (es++es', cfg'') 
pgTOP (SV id ty) (i,t,q,s,v,ctx)    =   ([(i, AcSto s, Q q)], (Q q, t, q+1,s+1,v,ctx))   
pgTOP (EV id ty) (i,t,q,s,v,ctx)    =   undefined 


pgParams :: [Param] -> Config -> Edges
pgParams []     cfg                 =   ([], cfg)
pgParams (p:ps) cfg                 =   (es ++ ess, cfg'') 
                                where   (es, cfg' ) = pgParam  p  cfg  
                                        (ess,cfg'') = pgParams ps cfg' 

pgParam :: Param -> Config -> Edges
pgParam (id,ty) (i,t,q,s,v,ctx)     =   ([(i, AcVar v, Q q)], (Q q, t, q+1,s,v+1,ctx))

pgBODY :: BODY -> Config -> Edges 
pgBODY (BODY _ ds tm _) cnf         =   (es++es', cnf'') 
                                where   (es,  cnf' ) = pgDecls ds cnf  
                                        (es', cnf'') = pgTerm  tm cnf' 

pgDecls :: [Decl] -> Config -> Edges 
pgDecls [] cnf = ([],cnf)
pgDecls (d:ds) cnf                  =   (es++ess, cnf'') 
                                where   (es,  cnf' )    = pgDecl  d  cnf
                                        (ess, cnf'')    = pgDecls ds cnf'

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
    if arglen == 0 
        then    ([], (i,t,q,s,v, ctx ++ [(0,Qe,Qe)])) 
        else 
                let (q0,q1) = (Q  q   , Q (q+1)) in 
                let (qn,qx) = (Q (q+2), Q (q+3)) in 
                let cfg     = (qn,qx,(q+4),s,v,(arglen,qn,qx):ctx) in 
                ([(i, AcSkip, q0),(q1, AcSkip, t)], cfg)  

pgDecl :: Decl -> Config -> Edges 
pgDecl (FLET id ps tm _) (i,t,q,s,v,ctx) = 
    let arglen      = length ps in 
    let fparams     = higherParams ps in 
    let funs        = (id, arglen) : fparams in  
    let (es,cfg)    = pgFuns funs (i,t,q,s,v,ctx) in 
    let (es',cfg')  = pgTerm tm cfg in 
    (es++es',cfg) 
pgDecl ( LET id    tm tm') (i,t,q,s,v,ctx) = pgDecl (FLET id [] tm tm') (i,t,q,s,v,ctx) 
pgDecl (SLET id    tm _) (i,t,q,s,v,ctx)   = ([], (i,t,q,s,v,ctx) )
-- here, tm cannot always be AExp a 
-- so this cannot be translated into " x := a "  
-- it should be like 
-- pg(tm ; x := retvalue) ; 
pgDecl (DATA id is ds  ) (i,t,q,s,v,ctx) = undefined 

fst3 (x,y,z) = x 

pgArgs :: [Term] -> Config -> [Int] -> Int -> Edges 
pgArgs []       cfg             ns                 k = ([],cfg) 
pgArgs (tm:tms)             (i,t,q,s,v,ctx) (0:ns) k = 
    let (e,(_,_,q',s',v',ctx'))         = pgTerm tm  (i,Q q,q+1,s,v,ctx)  in 
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


pgTermApp :: Term -> Config -> [Term] -> Edges 
pgTermApp (RED TmAPP [RED(TmVAR n)[],t2]) (i,t,q,s,v,ctx) cont = 
    let Just (argnum,qn,qx)         = searchFun n ctx in 
    let eenter                      = [(i     , AcEnter     , Q q     )] in 
    let argnums                     = map fst3 ctx in 
    let (econt,(_,_,q',s',v',ctx')) = pgArgs(t2:cont)(Q q,Q(q+1),q+2,s,v,ctx)argnums 1 in
    let erecord                     = [(Q(q+1), AcRecord i t, qn      )] in 
    let echeck                      = [(qx    , AcCheck  i t, Q q'    )] in 
    let eexit                       = [(Q q'  , AcExit      , Q (q'+1))] in 
    (eenter ++ econt ++ erecord ++ echeck ++ eexit, (i,t,q'+2,s',v',ctx'))  
pgTermApp (RED TmAPP [t1,t2]) cfg cont = pgTermApp t1 cfg (t2:cont) 


pgTerm :: Term -> Config -> Edges 
pgTerm (RED TmAPP [t1,t2]) cfg  = pgTermApp (RED TmAPP [t1,t2]) cfg []
pgTerm (RED TmIF [b,t1,t2]) (i,t,q,s,v,ctx) = 
    let (eb,(_,_,q' ,s' ,v' ,ctx' ))    = pgCond b  (i, Q q,q+2  ,s  ,v  ,ctx  ) in 
    let (e1,(_,_,q'',s'',v'',ctx''))    = pgTerm t1 (Q q   ,t,q' ,s' ,v' ,ctx' ) in 
    let (e2,cfg)                        = pgTerm t2 (Q(q+1),t,q'',s'',v'',ctx'') in 
    let eelse                           = [(i, AcSkip, Q(q+1))] in 
    (eb++e1++e2++eelse, cfg)  
pgTerm (RED (TmU256 n) []) (i,t,q,s,v,ctx) = 
    ([(i, AcPush (Ox (toHex n)), t)], (i,t,q,s,v,ctx)) 
pgTerm (RED (TmVAR n) []) (i,t,q,s,v,ctx) = 
    ([(i, AcPush (Var (show n)), t)], (i,t,q,s,v,ctx))
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
pgBOP "==" x y = Eq x y
pgBOP "*"  x y = Mul x y
pgBOP "-"  x y = Sub x y
pgBOP "+"  x y = Add x y
pgBOP "<"  x y = Lt x y
pgBOP ">"  x y = Gt x y 

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
    
