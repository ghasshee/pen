-- Program Graph 


module PG where 

import GCLL 
import Type
import Term
import Tree
import AST



data Var  = X Int deriving (Show, Eq, Read) 
data Sto  = S Int deriving (Show, Eq, Read) 
data Node = Q Int 
          | Qi | Qt
                deriving (Show, Eq, Read) 

data Action     = AcStop 
                | AcRevert EXPR EXPR
                | AcReturn EXPR EXPR
                | AcPop 
                | AcPush EXPR
                | AcSwap Int 
                | AcDup  Int 
                | AcCalldatacopy EXPR EXPR EXPR 
                | AcCodecopy EXPR EXPR EXPR
                | AcExtcodecopy 
                -- | AcSeq [Action] 
                | AcSkip                -- correspond to GOTO 
                | AcAssgin Var EXPR
                | AcBool   EXPR         -- correspond to IFGOTO
                | AcEnter               -- make empty frame 
                | AcExit                -- Return stacktop, remove frame, push stacktop
                | AcVar Int
                | AcSto Int
                | AcArray Var Int
                | AcRecord Node Node    -- 
                | AcCheck Node Node 
                


type Edge = (Node, Action, Node) 
type NewNode = Int
type NewSto = Int 
type NewVar = Int
type InitNode = Node 
type LastNode = Node
type ArgNum   = Int
data Bind   = FunBind (ArgNum, InitNode, LastNode) 
            | VarBind 
type FunCtx = [(ArgNum, InitNode, LastNode)] 
type Config = (InitNode, LastNode, NewNode, NewSto, NewVar, FunCtx) 
type Edges  = ([Edge], Config) 

pgCN :: CONTRACT -> Config -> Edges
pgCN (CN id tops) config = pgTOPs tops config

pgTOPs :: [TOP] -> Config -> Edges 
pgTOPs [] cnf = ([], cnf)
pgTOPs (top:tops) cnf = let (es,cnf') = pgTOP top cnf in 
                        let (ess,cnf'') = pgTOPs tops cnf' in 
                                                (es ++ ess, cnf'') 
    


pgTOP :: TOP -> Config -> Edges
pgTOP (MT id ty ps bd) cnf          =   let (es,cnf') = pgParams ps cnf in 
                                        pgBODY bd cnf' 
pgTOP (SV id ty) (i,t,q,s,v,ctx)    =   ([(i, AcSto s, Q q)], (Q q, t, q+1,s+1,v,ctx))   
pgTOP (EV id ty) (i,t,q,s,v,ctx)    =   undefined 


pgParams :: [Param] -> Config -> Edges
pgParams []     cnf                 =   ([], cnf)
pgParams (p:ps) cnf                 =   (es ++ ess, cnf'') 
                                where   (es, cnf' ) = pgParam  p  cnf  
                                        (ess,cnf'') = pgParams ps cnf' 

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

higherParams [] = [] 
higherParams ((id,ty):xs)   = if degreeOfFun ty == 0 
                                    then higherParams xs 
                                    else (id,degreeOfFun ty):higherParams xs

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


pgDecl (SLET id    tm _) (i,t,q,s,v,ctx) = undefined 
pgDecl ( LET id    tm _) (i,t,q,s,v,ctx) = undefined 
-- here, tm cannot always be AExp a 
-- so this cannot be translated into " x := a "  
-- it should be like 
-- pg(tm ; x := retvalue) ; 
pgDecl (DATA id is ds  ) (i,t,q,s,v,ctx) = undefined 


pgTerm :: Term -> Config -> Edges 
pgTerm (RED TmAPP [t1,t2]) (i,t,q,s,v,ctx) = 
    let eval    = undefined in 
    let f       = eval ctx in 
    undefined 

