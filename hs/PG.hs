-- Program Graph 


module PG where 

import Config
import GCLL 
import Node 
import Edge hiding (partition)
import Type
import Param
import Term
import Tree
import AST
import Hex
import Action  hiding (Var) 
import Data
import Datatype
import Utils

import Data.List (partition) 

import Prelude hiding (partition) 



pgFunCtxs   :: Config -> [FunOrArg ParamArity] -> Config 
pgFunCtxs   = foldl pgFunCtx 

pgFunCtx    :: Config -> FunOrArg ParamArity -> Config 
pgFunCtx cfg@(i,t,q,s,v,cx,sx) varInfo = let c:cs = cx in case varInfo of 
    Arg(id,0)           -> (i, t, q,  s,v, cx', sx) where 
            arg     = Arg(0, i, t) 
            cx'    = (arg:c) : cs 
    Arg(id,arity)       -> (qn,qx,q+2,s,v, cx', sx) where 
            (qn,qx) = (Q q, Q(q+1))
            arg     = Arg(arity, qn, qx)
            cx'    = (arg:c) : cs     
    Fun(id,arity)       -> (qn,qx,q+2,s,v, cx', sx) where 
            (qn,qx) = (Q q, Q(q+1)) 
            fun     = Fun(arity, qn, qx) 
            cx'    = [fun] : cx  



-- PROGRAM GRAPH with Configure 
type PG  = ([Edge(Node Int)Action], Config) 
type PG' = ([Edge(Node Int)Action], Config')



initialConfig :: Config
initialConfig = (Q 1,Q 2, 3, 0, 0, [], []) 

initialPG :: PG 
initialPG = ([], initialConfig) 

pg :: CONTRACT -> PG
pg = pgCN initialPG  



-------------------------
--    Program Graph    --  
-------------------------


pgCN pg (CN id tops) = pgTOPs pg tops 

pgTOPs                                  =   foldl pgTOP 
pgTOP (es,(i,t,q,s,v,cx,sx))  top       =   case top of 
    (SV id ty)          ->  (es ++ e, cfg') where 
        e       = [(i,AcPush(Ox 0),Q q),(Q q,AcPush(Ox(to s)),Q(q+1)),(Q(q+1), AcSstore, Q(q+2))]
        cfg'    = (Q(q+2),t,q+3,s+1,v,cx,id:sx)
    (MT id ty ps bd)    ->  (es'', (i,t,q',s',v',cx',sx)) where  -- (i, t)  must NOT change 
        id'                             =   id  ++  showTyParams ps 
        es'                             =   es ++ [(i,AcDispatch id', Q q), (Q(q+1), AcSkip, t) ]  
        (es'',(_,_,q',s',v',cx',_))     =   pgMT (es',(Q q,Q(q+1),q+2,s,v,cx,sx)) (MT id ty ps bd)
    
pgMT   pg (MT id ty ps bd)              =   pgBODY (pgParams pg ps) bd 

pgParams                                =   foldl pgParam
pgParam (es,(i,t,q,s,v,cx,sx)) _        =   (es ++ [(i,AcVar v,Q q)],(Q q,t,q+1,s,v+1,cx,sx))

pgBODY (es,cfg) (BODY _ ds tm _)        =   (es'', (i',t',q',s',v',cx',sx'))  where   
    (es' ,  (i,t,q,s,v,cx,sx))          = pgDecls (es,cfg) ds  
    (es'',  (i',t',q',s',v',cx',sx',_)) = pgTerm (es', (i,t,q,s,v,[]:cx,sx,0)) tm 

pgDecls                                 = foldl pgDecl
pgDecl (es, cfg@(i,t,q,s,v,cx,sx)) d    = case d of 
    LET  id ty tm fm        ->  pgDecl (es,cfg) (FLET id [] ty tm fm)  
    SLET id _  tm _         ->  (es' ++ e ,  cfg'  ) where   
        (es', (i',t',q',s',v',cx',sx',_))           = pgTerm (es, (i,Q q,q+3,s,v,[]:cx,id:sx,0)) tm  
        e                                           = [(Q q, AcPush (Ox (to n)), Q(q+1)), (Q(q+1),AcSstore,Q(q+2))] 
        cfg'                                        = (Q(q+2),t,q',s',v',cx,sx')
        Just n                                      = searchSto (drop 1 id) sx' 
    FLET id ps _ tm _       ->  (es',   (i,t,q''',s''',v''',cx'',sx'''))  where  -- (i,t) must NOT change
        arities                                     = paramArities  ps       
        cfg'@(_,_,_,_,_,cx',_)                      = pgFunCtxs cfg  [Fun (id,len ps)] 
        Fun (_,qn,qx)                               = searchFun 0 cx' 
        (_ ,_ ,q'',s'',v'',cx'',sx'')               = pgFunCtxs cfg' (Arg <$> arities) 
        (es',(_,_,q''',s''',v''',cx''',sx''',_))    = pgTerm (es, (qn,qx,q'',s'',v'',cx'',sx'',0)) tm 


pgTerm :: PG' -> Term -> PG' 
pgTerm (es,cfg@(i,t,q,s,v,cx,sx,d)) tr = 
    let cs:css  = cx                       in 
    let sx'    = reduced sx               in 
    let cfg_    = (i,t,q,s,v,cx,sx,d+1)   in 
    case tr of 
    RED TmAPP [t1,t2]                   ->  pgTermApp (es, cfg) tr [] 
    RED (TmU256 n) []                   ->  (es ++ [(i, AcPush (Ox n           ), t)], cfg_) where  
    RED (TmDATA n) []                   ->  (es ++ [(i, AcPush (Ox (data2nat n)), t)], cfg_) where 
    RED (TmVAR  n) []                   ->  case searchFun n cx of 
        Arg (0, qn, qx)                     -> (es ++ [(i, AcDup n',    t)], cfg_) where 
            n'                                      = calc_arglen cx n - n + d  
        Arg (_, qn, qx)                     -> err "Dynamical Higher Order Function (Function Closure as Value) is prohibited."
        Fun (0, qn, qx)                     -> (es ++ [(i, AcRecord i t, qn), (qx, AcCheck i t, t)], cfg_) 
        Fun (_, qn, qx)                     -> (es ++ [(i, AcSkip, qn), (qx, AcSkip, t)], cfg_) 
    RED TmIF [b,t1,t2]                  ->  pgTerm (es'', (Q(q+1),t, q'',s'',v'',cx'', sx'',d''-1)) t2 where 
        (es',(_,_,q' ,s' ,v' ,cx' ,sx' ,d'))      = pgCond (es,   (i,Q q,q+2,s,v,cx,sx,d)) b
        (es'',(_,_,q'',s'',v'',cx'',sx'',d''))    = pgTerm  (es'++enotb, (Q q,t, q' ,s',v',cx', sx',d')) t1
        enotb                                       = [(i, AcSkip, Q(q+1))]    
    RED (TmBOP o)[t1,t2]                ->  (es'' ++ [(Q(q+1),AcBop o,t)], (i'',t'',q'',s'',v'',cx'',sx'',d''-1))  where 
        (es',(i',t',q',s',v',cx',sx',d'))         = pgTerm (es ,(i,Q q,q+2,s,v,cx,sx,d)) t1
        (es'',(i'',t'',q'',s'',v'',cx'',sx'',d''))= pgTerm (es',(Q q,Q(q+1),q',s',v',cx',sx',d')) t2 
    RED (TmSTO n) [] | n >= len sx'    ->  (es ++ [(i, AcSto (len sx            - n - 1), t)], cfg_) 
    RED (TmSTO n) [] | sx /= sx'      ->  (es ++ [(i, AcSto (len sx - len sx' - n - 1), t)], cfg_)  
    RED (TmSTO n) []                    ->  (es ++ [(i, AcSto (len sx            - n - 1), t)], cfg_)  
    e                                   ->  error $ show e 


{-- 
 - Function Execution (or Function Call ) 
 - 1. push Arguments and PUSH "Record A"  
 - 2. goto Function Init Node 
 - 3. Function Execution
 - 4. if the top element of the Record/check stack is "Record A" then exit Function Terminal Node. This process is named "CHECK A".
 - 5. exit. 
 --} 


pgTermApp :: PG' -> Term -> [Term] -> PG' 
pgTermApp (es,cfg@(i,t,q,s,v,cx,sx,d)) (RED TmAPP [t1,t2]) cont = case t1 of 
    RED (TmVAR n) []    ->  case searchFun n cx of 
        Fun(0,_,_)          -> error $ "pgTermApp: illegal TmVAR " ++ show n ++ ":Fun " ++ show cx
        Arg(0,_,_)          -> error $ "pgTermApp: illegal TmVAR " ++ show n ++ ":Arg " ++ show cx
        Arg(_,qn,qx){--f@f(f x)--}  ->  pgArgs  (es++eent++ercd++echk++eexit,(Q q,Q(q+1),q+3,s,v,cx,sx,d)) (t2:cont) where 
            eent                =   [(i     , AcEnter     , Q q     )] 
            ercd                =   [(Q(q+1), AcRecord i t, qn      )]
            echk                =   [(qx    , AcCheck  i t, Q(q+2)  )] 
            eexit               =   [(Q(q+2), AcExit      , t       )] 
        Fun(_,qn,qx){-- double --}  ->  pgArgs' (es++eent++ercd++echk++eexit,(Q q,Q(q+1),q+3,s,v,cx,sx,d)) argcxs where 
            args                =   t2 : cont 
            acx                =   searchArgs n cx 
            argcxs             =   zip args acx 
            eent                =   [(i     , AcEnter     , Q q     )] 
            ercd                =   [(Q(q+1), AcRecord i t, qn      )] 
            echk                =   [(qx    , AcCheck  i t, Q(q+2)  )]  
            eexit               =   [(Q(q+2), AcExit      , t       )] 
    _                   -> pgTermApp (es,cfg)  t1 (t2:cont)


no_varg :: [(Term,Var)] -> Bool 
no_varg []                  = True 
no_varg ((_,Arg(0,_,_)):_ ) = False  
no_varg ((_,Arg(_,_,_)):cs) = no_varg cs

free_var :: VarCtx -> Int -> Bool 
free_var cx n = n >= len (hd cx) 


pgArgs' :: PG' -> [(Term,Var)] -> PG' 
pgArgs' pg []                                                      = pg 
pgArgs' (es,cfg@(i,t,q,s,v,cx@(cs:css),sx,d)) (argcx:argcxs)  =  
    let cfg_    = (i,t,q,s,v,cx,sx,d+1)        in 
    let calc n  = calc_arglen cx n - n + d     in 
    case argcx of 
    (RED(TmVAR n)[],Arg(0,_,_)) | free_var cx n    ->  (es ++ e,cfg) where 
        Fun(0,qn,qx)                =  searchFun n cx 
        e                           =  [(i,AcSkip,qn), (qx,AcSkip,t)] 
    (RED(TmVAR n)[],Arg(0,_,_)) | no_varg argcxs   ->  pgArgs' (es ++ earg, cfg_) argcxs where 
        earg                        = [(i, AcDup(calc n),  t)]
    (RED(TmVAR n)[],Arg(0,_,_))                     ->  pgArgs' (es ++ earg, cfg') argcxs where 
        earg                        = [(i, AcDup(calc n),Q q)]
        cfg'                        = (Q q,t,q+1,s,v,cx,sx,d+1)  
    (tm,            Arg(0,_,_)) | no_varg argcxs   ->  pgArgs' (pgTerm (es,cfg) tm) argcxs 
    (tm,            Arg(0,_,_))                     ->  pgArgs' (es',(Q q,t,q',s',v',cx',sx',d')) argcxs where 
        (es',(_,_,q',s',v',cx',sx',d'))  = pgTerm  (es,(i,Q q,q+1,s,v,cx,sx,d)) tm  
    -- Higher Order Args 
    (RED(TmVAR n)[],Arg(l,qn,qx))                   ->  pgArgs' (es ++ earg, cfg_) argcxs where 
        Fun(_,qn',qx')              = searchFun n cx   
        earg                        = [(qn, AcRecord i t, qn'), (qx', AcCheck i t, qx)] 
    e -> error $ "pgArgs: not supported arg " ++ show e 


pgArgs :: PG' -> [Term] -> PG' 
pgArgs pg [tm]                              =   pgTerm pg tm 
pgArgs (es,(i,t,q,s,v,cx,sx,d)) (tm:tms)    =   pgArgs (es',(Q q,t,q' ,s',v',cx',sx',d')) tms where 
    (es',(_,_,q',s',v',cx',sx',d'))         =   pgTerm (es, (i,Q q,q+1,s ,v ,cx ,sx ,d )) tm              


-- possibility 
-- 1. in function defintion,  variable call
-- 2. in function defintion,  function 
-- 3. in function call     ,  defined function variable 
-- 4. in function call     ,  just value 


pgCond :: PG' -> Term -> PG' 
pgCond (es, cfg@(i,t,q,s,v,cx,sx,d)) tm = (es ++ econd' , (i,t,q',s',v',cx,sx,d'-1)) where 
    econd'                                      = [(i,AcCond,Q q)] ++ econd ++ [(Q(q+1),AcDonc,t)]  
    (econd,(i',t',q',s',v',cx',sx',d'))       = pgTerm ([], (Q q, Q(q+1), q+2,s,v,cx,sx,d)) tm 




{--
_pgCond :: PG' -> Term -> PG' 
_pgCond (es, cfg@(i,t,q,s,v,cx,sx,ds)) (RED (TmBOP op) [t1,t2]) = 
    (es ++ [(i,AcBool ((pgBOP op) t1' t2'), t)], (i,t,q,s,v,cx,sx,ds)) where 
        t1' = tm2exp cx t1
        t2' = tm2exp cx t2 

pgBOP "!=" x y = Not (Eq x y)  
pgBOP "==" x y = Eq  x y
pgBOP "&&" x y = And x y 
pgBOP "||" x y = Or  x y 
pgBOP "*"  x y = Mul x y
pgBOP "-"  x y = Sub x y
pgBOP "+"  x y = Add x y
pgBOP "<"  x y = Lt  x y
pgBOP ">"  x y = Gt  x y 


tm2exp :: VarCtx -> Term -> EXPR 
tm2exp cx (RED (TmVAR  n) [])  =   Var (show n)  
tm2exp cx (RED (TmSTO  n) [])  =   GCLL.S (Ox (to n))
tm2exp cx (RED (TmU256 n) [])  =   Ox n
tm2exp cx (RED (TmU8   n) [])  =   Ox (to n)
tm2exp cx (RED (TmDATA d) [])  =   Ox (data2nat d)
tm2exp cx (RED (TmBOP o)[x,y]) =   pgBOP o (tm2exp cx x) (tm2exp cx y)
tm2exp cx (RED (TmAPP  )[x,y]) =   App (tm2exp cx x) (tm2exp cx y)
--}
    
