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
import DupDepth 
import Utils

import Data.Tuple.Extra (fst3, snd3, thd3) 
import Data.List (partition) 

import Prelude hiding (partition) 




pgFunCtxs   :: Config -> [FunOrArg ParamArity] -> Config 
pgFunCtxs   = foldl pgFunCtx 

pgFunCtx    :: Config -> FunOrArg ParamArity -> Config 
pgFunCtx cfg@(i,t,q,s,v,ctx,stx) varInfo = let c:cs = ctx in case varInfo of 
    Arg(id,0)           -> (i, t, q,  s,v, ctx', stx) where 
            arg     = Arg(0, i, t) 
            ctx'    = (arg:c) : cs 
    Arg(id,arity)       -> (qn,qx,q+2,s,v, ctx', stx) where 
            (qn,qx) = (Q q, Q(q+1))
            arg     = Arg(arity, qn, qx)
            ctx'    = (arg:c) : cs     
    Fun(id,arity)       -> (qn,qx,q+2,s,v, ctx', stx) where 
            (qn,qx) = (Q q, Q(q+1)) 
            fun     = Fun(arity, qn, qx) 
            ctx'    = [fun] : ctx  




-- PROGRAM GRAPH with Configure 
type PG  = ([Edge(Node Int)Action], Config) 
type PG' = ([Edge(Node Int)Action], Config')



initialConfig :: Config
initialConfig = (Q 1,Q 2, 3, 0, 0, [], []) 

initialPG :: PG 
initialPG = ([], initialConfig) 

pg :: CONTRACT -> PG
pg = _pgCN initialPG  




-------------------------
--    Program Graph    --  
-------------------------


_pgCN pg (CN id tops) = _pgTOPs pg tops 

_pgTOPs                                 =   foldl _pgTOP 
_pgTOP (es,(i,t,q,s,v,ctx,stx))  top    =   case top of 
    (SV id ty)          ->  (es ++ e, cfg') where 
        e       = [(i,AcPush(Ox 0),Q q),(Q q,AcPush(Ox(to s)),Q(q+1)),(Q(q+1), AcSstore, Q(q+2))]
        cfg'    = (Q(q+2),t,q+3,s+1,v,ctx,id:stx)
    (MT id ty ps bd)    ->  (es'', (i,t,q',s',v',ctx',stx)) where  -- (i, t)  must NOT change 
        id'                             =   id  ++  showTyParams ps 
        es'                             =   es ++ [(i,AcDispatch id', Q q), (Q(q+1), AcSkip, t) ]  
        (es'',(_,_,q',s',v',ctx',_))    =   _pgMT (es',(Q q,Q(q+1),q+2,s,v,ctx,stx)) (MT id ty ps bd)
    
_pgMT   pg (MT id ty ps bd)             =   _pgBODY (_pgParams pg ps) bd 

_pgParams                               =   foldl _pgParam
_pgParam (es,(i,t,q,s,v,ctx,stx)) _     =   (es ++ [(i,AcVar v,Q q)],(Q q,t,q+1,s,v+1,ctx,stx))

_pgBODY (es,cfg) (BODY _ ds tm _)       =   (es'', (i',t',q',s',v',ctx',stx'))  
                                where   (es' ,  (i,t,q,s,v,ctx,stx))          = _pgDecls (es,cfg) ds  
                                        (es'',  (i',t',q',s',v',ctx',stx',_)) = _pgTerm (es', (i,t,q,s,v,[]:ctx,stx,[])) tm 

_pgDecls                                = foldl _pgDecl
_pgDecl (es, cfg@(i,t,q,s,v,ctx,stx)) d = case d of 
    LET id ty tm fm         ->  _pgDecl (es,cfg) (FLET id [] ty tm fm)  
    SLET id  _  tm _        ->  (es' ++ e ,  cfg'  ) where   
        (es', (i',t',q',s',v',ctx',stx',_))         = _pgTerm (es, (i,Q q,q+3,s,v,[]:ctx,id:stx,[])) tm  
        e                                           = [(Q q, AcPush (Ox (to n)), Q(q+1)), (Q(q+1),AcSstore,Q(q+2))] 
        cfg'                                        = (Q(q+2),t,q',s',v',ctx',stx')
        Just _n                                     = searchSto id stx'  
        Just n                                      = searchSto (drop 1 id) stx' 
    FLET id ps _ tm _       ->  (es',   (i,t,q''',s''',v''',ctx'',stx'''))  where  -- (i,t) must NOT change
        arities                                     = paramArities  ps       
        cfg'@(_,_,_,_,_,ctx',_)                     = pgFunCtxs cfg  [Fun (id,len ps)] 
        Fun (_,qn,qx)                               = searchFun 0 ctx' 
        (_ ,_ ,q'',s'',v'',ctx'',stx'')             = pgFunCtxs cfg' (Arg <$> arities) 
        (es',(_,_,q''',s''',v''',ctx''',stx''',_))  = _pgTerm (es, (qn,qx,q'',s'',v'',ctx'',stx'',[])) tm 


_pgTerm :: PG' -> Term -> PG' 
_pgTerm (es,cfg@(i,t,q,s,v,ctx,stx,ds)) tr = 
    let ds'     = d_minus ds                in 
    let cfg'    = (i,t,q,s,v,ctx,stx,ds')   in 
    let cs:css  = ctx                       in 
    case tr of 
    RED TmAPP [t1,t2]                   ->  (es ++ eapp, cfg'') where 
        cfg'                                    = (i,t,q,s,v,ctx,stx)
        (eapp, cfg'')                           = pgTermApp tr cfg' [] (d_push 2 ds) 
    RED (TmU256 n) []                   ->  (es ++ [(i, AcPush (Ox n           ), t)], cfg') where  
    RED (TmDATA d) []                   ->  (es ++ [(i, AcPush (Ox (data2nat d)), t)], cfg') where 
    RED (TmVAR  n) []                   ->  let d = dup_sum ds in 
                                            case searchFun (n+d) ctx of 
        Arg (0, qn, qx)                    -> (es ++ [(i, AcDup n',    t)], cfg') where 
            n'                                  = calc_arglen ctx (n+d) - n 
            cfg'                                = (i,t,q,s,v,ctx',stx,d_minus (d_dup ds))  
            ctx'                                = (Arg (0,qn,qx) : cs) : css  -- Because of !! DUP !! opearation the context gains 
        Arg (n, qn, qx)                    -> err "Dynamical Higher Order Function (Function Closure as Value) is prohibited."
        Fun (0     , qn, qx)               -> (es ++ [(i, AcSkip, qn), (qx, AcSkip, t)], cfg') where 
        Fun (argnum, qn, qx)               -> (es ++ [(i, AcSkip, qn), (qx, AcSkip, t)], cfg') where 
    RED TmIF [b,t1,t2]                  ->  (es ++ eb++e1++enotb++e2, cfg')  where 
        (eb,(_,_,q' ,s' ,v' ,ctx' ,stx' ,ds' )) = pgCond b  (i,   Q q, q+2,s  ,v  ,ctx  , stx) (d_push 3 ds)
        (e1,(_,_,q'',s'',v'',ctx'',stx'',ds'')) = pgTerm t1 (Q q   ,t, q' ,s' ,v' ,ctx' , stx') ds'
        enotb                                   = [(i, AcSkip, Q(q+1))]    
        (e2,cfg')                               = pgTerm t2 (Q(q+1),t, q'',s'',v'',ctx'', stx'') ds'' 
    RED (TmBOP o)[t1,t2]                ->  (es ++ e1++e2++[(Q(q+1),AcBop o,t)],cfg')  where 
        (e1,(i',t',q',s',v',ctx',stx',ds'))     = pgTerm t1 (i,  Q q,   q+2,s,v,ctx,stx) (d_push 2 ds)
        (e2,cfg')                               = pgTerm t2 (Q q,Q(q+1),q',s',v',ctx',stx') ds' 
    RED (TmSTO n) [] | n >= len stx'    ->  (es ++ [(i, AcSto n' , t)], cfg') where 
        n'                                      = len stx - n - 1 
        stx'                                    = reduced stx 
        cfg'                                    = (i,t,q,s,v, ctx,stx,d_minus ds)  
    RED (TmSTO n) [] | stx /= stx'      ->  (es ++ [(i, AcSto (len stx - n' - 1), t)], cfg') where 
        n'                                      = n + len stx'
        stx'                                    = reduced stx 
    RED (TmSTO n) []                    ->  (es ++ [(i, AcSto (len stx - n  - 1), t)], cfg' ) where 
    _                                   ->  (es , cfg') where 




pgTerm :: Term -> Config -> DupDepth -> PG' 
pgTerm tr cfg@(i,t,q,s,v,ctx,stx) ds    = case tr of 
    RED TmAPP [t1,t2]                   ->  pgTermApp tr cfg [] (d_push 2 ds) 
    RED (TmU256 n) []                   ->  ([(i, AcPush (Ox n           ), t)], cfg') where  
        cfg'                                    = (i,t,q,s,v,ctx,stx, d_minus ds)
    RED (TmDATA d) []                   ->  ([(i, AcPush (Ox (data2nat d)), t)], cfg') where 
        cfg'                                    = (i,t,q,s,v,ctx,stx, d_minus ds)
    RED (TmVAR  n) []                   ->  let d = dup_sum ds in 
                                            case searchFun (n+d) ctx of 
        Arg (0, qn, qx)                    -> ([(i, AcDup n',    t)], cfg') where 
            n'                                  = calc_arglen ctx (n+d) - n 
            ds'                                 = d_dup ds 
            cfg'                                = (i,t,q,s,v,ctx',stx,d_minus ds')  
            cs:css                              = ctx 
            ctx'                                = (Arg (0,qn,qx) : cs) : css  -- Because of !! DUP !! opearation the context gains 
        Arg (n, qn, qx)                    -> error "Cannot Call Higher Order Variable as value. Call as Function"
        Fun (0     , qn, qx)               -> ([(i, AcSkip, qn), (qx, AcSkip, t)], cfg') where 
            cfg'                                = (i,t,q,s,v, ctx,stx,d_minus ds)  
        Fun (argnum, qn, qx)               -> ([(i, AcSkip, qn), (qx, AcSkip, t)], cfg') where 
            cfg'                                = (i,t,q,s,v, ctx,stx,d_minus ds)  
    RED TmIF [b,t1,t2]                  ->  (eb++e1++enotb++e2, cfg')  where 
        (eb,(_,_,q' ,s' ,v' ,ctx' ,stx' ,ds' )) = pgCond b  (i,   Q q, q+2,s  ,v  ,ctx  , stx) (d_push 3 ds)
        (e1,(_,_,q'',s'',v'',ctx'',stx'',ds'')) = pgTerm t1 (Q q   ,t, q' ,s' ,v' ,ctx' , stx') ds'
        enotb                                   = [(i, AcSkip, Q(q+1))]    
        (e2,cfg')                               = pgTerm t2 (Q(q+1),t, q'',s'',v'',ctx'', stx'') ds'' 
    RED (TmBOP o)[t1,t2]                ->  (e1++e2++[(Q(q+1),AcBop o,t)],cfg')  where 
        (e1,(i',t',q',s',v',ctx',stx',ds'))     = pgTerm t1 (i,  Q q,   q+2,s,v,ctx,stx) (d_push 2 ds)
        (e2,cfg')                               = pgTerm t2 (Q q,Q(q+1),q',s',v',ctx',stx') ds' 
    RED (TmSTO n) [] | n >= len stx' ->  ([(i, AcSto n' , t)], cfg') where 
        n'                                      = len stx - n - 1 
        stx'                                    = reduced stx 
        cfg'                                    = (i,t,q,s,v, ctx,stx,d_minus ds)  
    RED (TmSTO n) [] | stx /= stx'      ->  ([(i, AcSto (len stx - n' - 1), t)], cfg') where 
        n'                                      = n + len stx'
        stx'                                    = reduced stx 
        cfg'                                    = (i,t,q,s,v, ctx,stx,d_minus ds)  
    RED (TmSTO n) []                    ->  ([(i, AcSto (len stx - n  - 1), t)], cfg' ) where 
        cfg'                                    = (i,t,q,s,v, ctx,stx,d_minus ds)  
    _                                   ->  ([], cfg') where 
        cfg'                                    = (i,t,q,s,v, ctx,stx,d_minus ds) 



{-- Function Execution (or Function Call ) 
 - 1. push Arguments and PUSH "Record A"  
 - 2. goto Function Init Node 
 - 3. Function Execution
 - 4. if the top element of the Record/check stack is "Record A" then exit Function Terminal Node. This process is named "CHECK A".
 - 5. exit. --} 


pgTermApp :: Term -> Config -> [Term] -> DupDepth -> PG' 
pgTermApp (RED TmAPP [t1,t2]) cfg@(i,t,q,s,v,ctx,stx) cont ds = case t1 of 
    RED (TmVAR n) []    ->  let d = dup_sum ds in 
                            case searchFun (n+d) ctx of 
        Fun(0,_,_)          -> error $ "pgTermApp: illegal TmVAR " ++ show (n+d) ++ ":Fun " ++ show ctx
        Arg(0,_,_)          -> error $ "pgTermApp: illegal TmVAR " ++ show (n+d) ++ ":Arg " ++ show ctx
        Arg(argnum,qn,qx){-- f@f(f x) --}    -> (eent++econt++ercd++echk++eexit, cfg')  where 
            argnums             =   ((fst3 <$>)<$>)<$> ctx 
            eent                =   [(i     , AcEnter     , Q q     )] 
            (econt,cfg')        =   pgArgs(t2:cont)(Q q,Q(q+1),q+3,s,v,ctx,stx) argnums (d_minus ds)
            ercd                =   [(Q(q+1), AcRecord i t, qn      )]
            echk                =   [(qx    , AcCheck  i t, Q(q+2)  )] 
            eexit               =   [(Q(q+2), AcExit      , t       )] 
        Fun(argnum,qn,qx){-- double --} ->  
                (eent++econt++ercd++echk++eexit, cfg')  where 
            args                =   t2 : cont 
            actx                =   searchArgs (n+d) ctx 
            argctxs             =   zip args actx 
            (econt, cfg')       =   pgArgs' argctxs (Q q,Q(q+1),q+3,s,v,ctx,stx,d_minus ds)
            argnums             =   ((fst3 <$>) <$>) <$> ctx 
            eent                =   [(i     , AcEnter     , Q q     )] 
            ercd                =   [(Q(q+1), AcRecord i t, qn      )] 
            echk                =   [(qx    , AcCheck  i t, Q(q+2)  )]  
            eexit               =   [(Q(q+2), AcExit      , t       )] 

    _                   -> pgTermApp t1 cfg (t2:cont) ds



no_varg :: [(Term,Var)] -> Bool 
no_varg []                  = True 
no_varg ((_,Arg(0,_,_)):_ ) = False  
no_varg ((_,Arg(_,_,_)):cs) = no_varg cs

var_out_of_ctx :: VarCtx -> Int -> Bool 
var_out_of_ctx ctx n = n >= len (hd ctx) 


pgArgs' :: [(Term,Var)] -> Config' -> PG' 
pgArgs' []               cfg                                 = ([],cfg) 
pgArgs' (argctx:argctxs) cfg@(i,t,q,s,v,ctx@(cs:css),stx,ds) = 
    let d       = dup_sum ds    in 
    let ds'     = d_dup ds      in  
    case argctx of 
    (RED(TmVAR n)[], Arg(0,_,_)) | var_out_of_ctx ctx (n+d) -> (e,cfg) where 
        Fun(0,qn,qx)                =  searchFun (n+d) ctx 
        e                           =  [(i,AcSkip,qn), (qx,AcSkip,t)] 
        cfg                         =  (i,t,q,s,v,ctx,stx,d_minus ds)
    (RED(TmVAR n)[], Arg(0,qn,qx)) | no_varg argctxs  ->  (earg ++ eargs, cfg'') where 
        earg                        = [(i, AcDup n',    t)]
        n'                          = calc_arglen ctx (n+d) - n 
        cfg'                        = (i,t,q,s,v,ctx',stx,d_minus ds')  
        ctx'                        = (Arg (0,qn,qx) : cs) : css  -- Since DUP_ gains the stack, the context gains 
        (eargs, cfg'')              = pgArgs' argctxs cfg' 
    (RED(TmVAR n)[], Arg(0,qn,qx))                    ->  (earg ++ eargs, cfg'') where 
        earg                        = [(i, AcDup n',  Q q)]
        n'                          = calc_arglen ctx (n+d)- n 
        cfg'                        = (Q q,t,q+1,s,v,ctx',stx,d_minus ds')  
        ctx'                        = (Arg (0,qn,qx) : cs) : css  -- Since DUP_ gains the stack, the context gains 
        (eargs, cfg'')              = pgArgs' argctxs cfg' 
    (tm, Arg(0,qn,qx)) | no_varg argctxs  ->  (earg ++ eargs, cfg'') where 
        (earg, cfg'  )              = pgTerm  tm      (i,t,q,s,v,ctx,stx) ds 
        (eargs, cfg'')              = pgArgs' argctxs cfg' 
    (tm, Arg(0,qn,qx))                    ->  (earg ++ eargs, cfg'') where 
        (earg,(_,_,q',s',v',ctx',stx',ds')) = pgTerm  tm (i,Q q,q+1,s,v,ctx,stx) ds 
        (eargs, cfg'')              = pgArgs' argctxs   (Q q,t,q',s',v',ctx',stx',ds') 
    (RED(TmVAR n)[], Arg(l,qn,qx)) {-- Higher Order Variable --} ->  (earg ++ eargs, cfg') where 
        d                           = dup_sum ds 
        Fun(_,qn',qx')              = searchFun (n+d) ctx   
        earg                        = [(qn, AcRecord i t, qn'), (qx', AcCheck i t, qx)] 
        (eargs, cfg')               = pgArgs' argctxs (i,t,q,s,v,ctx,stx,d_minus ds) 
    e -> error $ "pgArgs: not supported arg " ++ show e 


pgArgs :: [Term] -> Config -> [[FunOrArg Int]] -> DupDepth -> PG' 
pgArgs []     (i,t,q,s,v,ctx,stx)            nss ds = ([],      (i,    t,q  ,s  ,v  ,ctx  ,stx,  ds)) 
pgArgs [tm]   (i,t,q,s,v,ctx,stx)((Arg _:ns):nss)ds = pgTerm tm (i,    t,q  ,s  ,v  ,ctx  ,stx ) ds
pgArgs(tm:tms)(i,t,q,s,v,ctx,stx)((Arg _:ns):nss)ds = (e++e',   (i,    t,q'',s'',v'',ctx'',stx'',ds'')) where 
    (e ,(_,_,q' ,s' ,v' ,ctx' ,stx' ,ds' ))         = pgTerm tm (i  ,Q q,q+1,s  ,v  ,ctx  ,stx ) ds             
    (e',(_,_,q'',s'',v'',ctx'',stx'',ds''))         = pgArgs tms(Q q,  t,q' ,s' ,v' ,ctx' ,stx') (ns:nss) ds'
pgArgs [tm]   (i,t,q,s,v,ctx,stx)([]        :nss)ds = (e,       (i  ,  t,q' ,s' ,v' ,ctx' ,stx', ds')) where 
    (e ,(_,_,q' ,s' ,v' ,ctx' ,stx' ,ds' ))         = pgTerm tm (i  ,  t,q  ,s  ,v  ,ctx  ,stx ) ds 
pgArgs(tm:tms)(i,t,q,s,v,ctx,stx)([]        :nss)ds = (e++e',   (i,    t,q'',s'',v'',ctx'',stx'',ds'')) where 
    (e ,(_,_,q' ,s' ,v' ,ctx' ,stx' ,ds' ))         = pgTerm tm (i  ,Q q,q+1,s  ,v  ,ctx  ,stx ) ds 
    (e',(_,_,q'',s'',v'',ctx'',stx'',ds''))         = pgArgs tms(Q q,  t, q',s' ,v' ,ctx' ,stx') ([]:nss) ds'
pgArgs [tm]   (i,t,q,s,v,ctx,stx)((Fun n:ns):nss)ds = (e,       (i  ,  t,q' ,s' ,v' ,ctx' ,stx', ds')) where 
    (e ,(_,_,q' ,s' ,v' ,ctx' ,stx' ,ds' ))         = pgTerm tm (i  ,  t,q  ,s  ,v  ,ctx  ,stx ) ds 
pgArgs(tm:tms)(i,t,q,s,v,ctx,stx)((Fun n:ns):nss)ds = (e++e',   (i,    t,q'',s'',v'',ctx'',stx'',ds'')) where 
    (e ,(_,_,q' ,s' ,v' ,ctx' ,stx' ,ds' ))         = pgTerm tm (i  ,Q q,q+1,s  ,v  ,ctx  ,stx ) ds 
    (e',(_,_,q'',s'',v'',ctx'',stx'',ds''))         = pgArgs tms(Q q,  t,q' ,s' ,v' ,ctx' ,stx') (ns:nss) ds'
    
--pgArgs (RED(TmVAR j)[]:tms) (i,t,q,s,v,ctx,stx) (n:ns)     k = pgArgs tms (i,t,q,s,v,ctx) ns (k+1)  
    -- Higher Order Variable is Skipped since We do not push values to the stack before function call.
    -- Rather, we connect edges. 

-- possibility 
-- 1. in function defintion,  variable call
-- 2. in function defintion,  function 
-- 3. in function call     ,  defined function variable 
-- 4. in function call     ,  just value 



pgCond :: Term -> Config -> DupDepth -> PG' 
pgCond (RED (TmBOP op) [t1,t2]) (i,t,q,s,v,ctx,stx) ds = 
    ([(i,AcBool ((pgBOP op) t1' t2'), t)], (i,t,q,s,v,ctx,stx,d_minus ds)) where 
        t1' = tm2exp ctx t1
        t2' = tm2exp ctx t2 

pgBOP "!=" x y = Not (Eq x y)  
pgBOP "==" x y = Eq  x y
pgBOP "&&" x y = And x y 
pgBOP "||" x y = Or  x y 
pgBOP "*"  x y = Mul x y
pgBOP "-"  x y = Sub x y
pgBOP "+"  x y = Add x y
pgBOP "<"  x y = Lt  x y
pgBOP ">"  x y = Gt  x y 


tm2exp :: a -> Term -> EXPR 
tm2exp ctx (RED (TmVAR  n) [])  =   Var (show n)  
tm2exp ctx (RED (TmSTO  n) [])  =   GCLL.S   (Ox (to n))
tm2exp ctx (RED (TmU256 n) [])  =   Ox n
tm2exp ctx (RED (TmDATA d) [])  =   Ox (data2nat d)
tm2exp ctx (RED (TmBOP o)[x,y]) =   pgBOP o (tm2exp ctx x) (tm2exp ctx y)

    
