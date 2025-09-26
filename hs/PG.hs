-- Program Graph 


module PG where 

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
type ArgLen     = Int 


{-- Function Context consits of 3 components 
 -  1. The number of Args
 -  2. Initial Node of Program Graph of the function 
 -  3. Terminal Node of Program Graph of the function --} 

data FunOrArg a = Fun a | Arg a deriving (Functor, Show) 
type Var        = FunOrArg (ArgNum, INode, TNode)
type VarCtx     = [[Var]] 
type StoCtx     = [ID] 

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


paramDegrees :: Params -> [FunOrArg (ID,ArgLen)] 
--paramDegrees []                     = [] 
--paramDegrees ((id,ty):xs)           = Arg (id,tyDegrees ty): paramDegrees xs

paramDegrees ps = Arg . (tyDegrees <$>) <$> ps 

tyDegrees :: Ty -> Int
tyDegrees (TyARR tyA tyB)         = tyDegrees tyB + 1 
tyDegrees _                       = 0  




-- VAR n |-> 
-- 1. dive in corresponding function context (loop)  
-- 2. calculate VAR n is k-th argment of the context 
-- 3. note that we do not count higher-order arguments. 
calc_arglen :: VarCtx -> Int -> Int 
calc_arglen ctx n = calc (loop ctx n (hd ctx)) where 
    loop          []  _  _      = error $ "calc_arglen: ctx := " ++ show ctx 
    loop         xss  0  cur    = cur 
    loop (   [] :xss) n  cur    = loop     xss   n   (hd xss) 
    loop ((x:xs):xss) n  cur    = loop (xs:xss) (n-1) cur 
    calc              []        = error $ "calc_arglen: cannot reach here"
    calc (Fun _      :xs)       = 0
    calc (Arg (0,_,_):xs)       = 1 + calc xs
    calc (Arg _      :xs)       = calc xs


pgFunCtxs :: [FunOrArg (ID,ArgLen)] -> Config -> Config 
pgFunCtxs l cfg = foldl pgFunCtx cfg l 
--pgFunCtxs []     cfg   =   cfg 
--pgFunCtxs (f:fs) cfg   =   pgFunCtxs fs (pgFunCtx f cfg)  

pgFunCtx :: Config -> FunOrArg (ID,ArgLen) -> Config 
pgFunCtx cfg@(i,t,q,s,v,ctx,stx) varInfo = let c:cs = ctx in case varInfo of 
    Arg(id,0)           -> (i, t, q,  s,v, ctx', stx) where 
            arg     = Arg(0, i, t) 
            ctx'    = (arg:c) : cs 
    Arg(id,alen)        -> (qn,qx,q+2,s,v, ctx', stx) where 
            (qn,qx) = (Q q, Q(q+1))
            arg     = Arg(alen, qn, qx)
            ctx'    = (arg:c) : cs     
    Fun(id,alen)        -> (qn,qx,q+2,s,v, ctx', stx) where 
            (qn,qx) = (Q q, Q(q+1)) 
            fun     = Fun(alen, qn, qx) 
            ctx'    = [fun] : ctx  


searchFun :: Int -> VarCtx -> Var 
searchFun i ctx     = loop i ctx where 
    loop 0 ([]:css)     = loop' 0 css
    loop 0 (cs:css)     = hd cs  
    loop n ([]:css)     = loop' n css
    loop n (cs:css)     = loop (n-1) (tl cs:css) 
    loop' 0 (cs:css)    = last cs  
    loop' n (cs:css)    = loop' (n-1) css
    loop' n []          = error $ "searchFun : out of context : " ++ show i ++ " : " ++ show ctx


searchSto :: ID -> StoCtx -> Maybe Int
searchSto s []                      = Nothing
searchSto s (x:xs)  | s == x        = Just (length xs)  
                    | otherwise     = searchSto s xs


searchArgs  i ctx   = loop i ctx where 
    loop  0 ([]:css)    = loop' 0 css
    loop  0 (cs:css)    = drop 1 $ reverse (hd ctx)  
    loop  n ([]:css)    = loop' n css
    loop  n (cs:css)    = loop (n-1) (tl cs:css) 
    loop' 0 (cs:css)    = if len cs <= 1 then err ("searchArgs " ++ show cs) else tl $ reverse cs
    loop' n (cs:css)    = loop' (n-1) css
    loop' n []          = err $ "searchFun : out of context : " ++ show i ++ " : " ++ show ctx









-- Configuration Type 
type Config     = (INode, TNode, FreshNode, FreshSto, FreshVar, VarCtx, StoCtx) 
type Config'    = (INode, TNode, FreshNode, FreshSto, FreshVar, VarCtx, StoCtx, DupDepth)


-- PROGRAM GRAPH with Configure 
type Edges  = ([Edge(Node Int)Action], Config) 
type Edges' = ([Edge(Node Int)Action], Config')


-- Initial Configuration  
initialConfig = (Q 1,Q 2, 3, 0, 0, [], []) 


pg :: CONTRACT -> Edges
pg cn = pgCN cn initialConfig  




-------------------------
--    Program Graph    --  
-------------------------

-- CN -> pg 
pgCN    :: CONTRACT -> Config -> Edges
pgCN (CN id tops) config    = pgTOPs tops config


-- [TOP] -> pg 
pgTOPs  :: [TOP] -> Config -> Edges 
pgTOPs  []   cfg@(i,t,_,_,_,_,_)    =   ([(i,AcSkip,t)], cfg)
pgTOPs  (top:tops)   cfg            =   (es ++ ess,      cfg'') where 
                                    (es, cfg' ) = pgTOP  top  cfg 
                                    (ess,cfg'') = pgTOPs tops cfg'  

-- TOP -> pg     
pgTOP :: TOP -> Config -> Edges
pgTOP (EV id ty)        (i,t,q,s,v,ctx,stx) =   undefined 
pgTOP (DT id ids cnstrs)(i,t,q,s,v,ctx,stx) =   undefined 
pgTOP (SV id ty)        (i,t,q,s,v,ctx,stx) =   (e, cfg') where 
    e       = [(i,AcPush(Ox 0),Q q),(Q q,AcPush(Ox (toInteger s)),Q(q+1)),(Q(q+1), AcSstore, Q(q+2))]
    cfg'    = (Q(q+2),t,q+3,s+1,v,ctx,id:stx)
pgTOP (MT id ty ps bd)  (i,t,q,s,v,ctx,stx) =   (e ++ edispatch, (i,t,q',s',v',ctx', stx)) where 
    edispatch                       = [(i,AcDispatch id', Q q), (Q(q+1), AcSkip, t) ] 
    (e,(_,_,q',s',v',ctx',_))       = pgMT (MT id ty ps bd) (Q q,Q(q+1),q+2,s,v,ctx, stx) 
    id'                             = id  ++  showTyParams ps 

pgMT :: TOP -> Config -> Edges
pgMT (MT id ty ps bd) cfg           =   (es++es', cfg'') 
                                where   (es ,cfg' ) = pgParams ps cfg   
                                        (es',cfg'') = pgBODY   bd cfg'   

pgParams :: [Param] -> Config -> Edges
pgParams []     cfg                 =   ([], cfg)
pgParams (p:ps) cfg                 =   (es ++ ess, cfg'') 
                                where   (es, cfg' ) = pgParam  p  cfg  
                                        (ess,cfg'') = pgParams ps cfg' 

pgParam :: Param -> Config -> Edges
pgParam (id,ty) (i,t,q,s,v,ctx,stx)     =   ([(i, AcVar v, Q q)], (Q q, t, q+1,s,v+1,ctx,stx))


-- BODY -> pg 
pgBODY :: BODY -> Config -> Edges 
pgBODY (BODY _ ds tm _) cfg         =   (es ++ es', (i',t',q',s',v',ctx',stx')) 
                                where   (es,  (i,t,q,s,v,ctx,stx))          = pgDecls ds cfg 
                                        (es', (i',t',q',s',v',ctx',stx',_)) = pgTerm  tm (i,t,q,s,v,[]:ctx,stx) []

pgDecls :: [Decl] -> Config -> Edges 
pgDecls []     cfg                  =   ([],cfg)
pgDecls (d:ds) cfg                  =   (es ++ ess, cfg'') 
                                where   (es, cfg')      = pgDecl d cfg
                                        (ess, cfg'')    = pgDecls ds cfg'  

-- Decl -> pg  
pgDecl :: Decl -> Config -> Edges 
pgDecl d cfg@(i,t,q,s,v,ctx,stx)    = case d of 
    LET id ty tm fm         ->  pgDecl (FLET id [] ty tm fm) cfg 
    SLET id  _  tm _        ->  (es ++ es',  cfg'  ) where   
        (es, (i',t',q',s',v',ctx',stx',_))          = pgTerm tm (i,Q q,q+3,s,v,[]:ctx, id:stx) []
        (es', cfg')                                 = ([(Q q, AcPush (Ox (toInteger n)), Q(q+1)), (Q(q+1),AcSstore,Q(q+2))], (Q(q+2),t,q',s',v',ctx',stx'))
        Just _n                                     = searchSto id stx'  
        Just n                                      = searchSto (drop 1 id) stx' 
    FLET id ps _ tm _       ->  (es,   (i,t,q''',s''',v''',ctx'',stx'))  where 
        arglen                                      = length        ps                    
        params                                      = paramDegrees  ps       
        (i',t',q',s',v',ctx',stx')                  = pgFunCtxs [Fun (id,arglen)] (i,t,q,s,v,ctx,stx)       
        Fun (_,qn,qx)                               = searchFun 0 ctx' 
        (_ ,_ ,q'',s'',v'',ctx'',stx'')             = pgFunCtxs params  (i',t',q',s',v',ctx',stx)       
        (es,(_,_,q''',s''',v''',ctx''',stx''',_))   = pgTerm tm   (qn,qx,q'',s'',v'',ctx'',stx'') []


pgTerm :: Term -> Config -> DupDepth -> Edges' 
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
    RED (TmSTO n) [] | n >= length stx' ->  ([(i, AcSto n' , t)], cfg') where 
        n'                                      = length stx - n - 1 
        stx'                                    = reduced stx 
        cfg'                                    = (i,t,q,s,v, ctx,stx,d_minus ds)  
    RED (TmSTO n) [] | stx /= stx'      ->  ([(i, AcSto (length stx - n' - 1), t)], cfg') where 
        n'                                      = n + length stx'
        stx'                                    = reduced stx 
        cfg'                                    = (i,t,q,s,v, ctx,stx,d_minus ds)  
    RED (TmSTO n) []                    ->  ([(i, AcSto (length stx - n  - 1), t)], cfg' ) where 
        cfg'                                    = (i,t,q,s,v, ctx,stx,d_minus ds)  
    _                                   ->  ([], cfg') where 
        cfg'                                    = (i,t,q,s,v, ctx,stx,d_minus ds) 


reduced []                     = [] 
reduced (('\'':_) : xs)        = reduced xs 
reduced stx                    = stx 




pgTermApp :: Term -> Config -> [Term] -> DupDepth -> Edges' 
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
            argctxs             =   zip3 [1..] args actx 
            (econt, cfg')       =   pgArgs' argctxs (Q q,Q(q+1),q+3,s,v,ctx,stx,d_minus ds)
            argnums             =   ((fst3 <$>) <$>) <$> ctx 
            eent                =   [(i     , AcEnter     , Q q     )] 
            ercd                =   [(Q(q+1), AcRecord i t, qn      )] 
            echk                =   [(qx    , AcCheck  i t, Q(q+2)  )]  
            eexit               =   [(Q(q+2), AcExit      , t       )] 

    _                   -> pgTermApp t1 cfg (t2:cont) ds

no_varg [] = True 
no_varg ((_,_,Arg(0,_,_)):ctx) = False  
no_varg ((_,_,Arg(_,_,_)):ctx) = no_varg ctx 

var_out_of_ctx ctx n = n >= length (hd ctx) 

pgArgs' :: [(Int,Term,Var)] -> Config' -> Edges' 
pgArgs' []                cfg                       = ([],cfg) 
pgArgs' (argctx:argctxs) cfg@(i,t,q,s,v,ctx@(cs:css),stx,ds) = 
    let d       = dup_sum ds    in 
    let ds'     = d_dup ds      in  
    case argctx of 
    (_, RED(TmVAR n)[], Arg(0,_,_)) | var_out_of_ctx ctx (n+d) -> (e,cfg) where 
        Fun(0,qn,qx)                =  searchFun (n+d) ctx 
        e                           =  [(i,AcSkip,qn), (qx,AcSkip,t)] 
        cfg                         =  (i,t,q,s,v,ctx,stx,d_minus ds)
    (_, RED(TmVAR n)[], Arg(0,qn,qx)) | no_varg argctxs  ->  (earg ++ eargs, cfg'') where 
        earg                        = [(i, AcDup n',    t)]
        n'                          = calc_arglen ctx (n+d) - n 
        cfg'                        = (i,t,q,s,v,ctx',stx,d_minus ds')  
        ctx'                        = (Arg (0,qn,qx) : cs) : css  -- Because of !! DUP !! opearation the context gains 
        (eargs, cfg'')              = pgArgs' argctxs cfg' 
    (_, RED(TmVAR n)[], Arg(0,qn,qx))                    ->  (earg ++ eargs, cfg'') where 
        earg                        = [(i, AcDup n',  Q q)]
        n'                          = calc_arglen ctx (n+d)- n 
        cfg'                        = (Q q,t,q+1,s,v,ctx',stx,d_minus ds')  
        ctx'                        = (Arg (0,qn,qx) : cs) : css  -- Because of !! DUP !! opearation the context gains 
        (eargs, cfg'')              = pgArgs' argctxs cfg' 
    (_, tm, Arg(0,qn,qx)) | no_varg argctxs  ->  (earg ++ eargs, cfg'') where 
        (earg, cfg'  )              = pgTerm  tm      (i,t,q,s,v,ctx,stx) ds 
        (eargs, cfg'')              = pgArgs' argctxs cfg' 
    (_, tm, Arg(0,qn,qx))                    ->  (earg ++ eargs, cfg'') where 
        (earg,(_,_,q',s',v',ctx',stx',ds')) = pgTerm  tm (i,Q q,q+1,s,v,ctx,stx) ds 
        (eargs, cfg'')              = pgArgs' argctxs   (Q q,t,q',s',v',ctx',stx',ds') 
    (_, RED(TmVAR n)[], Arg(l,qn,qx))        ->  (earg ++ eargs, cfg') where 
        d                           = dup_sum ds 
        Fun(_,qn',qx')              = searchFun (n+d) ctx   
        earg                        = [(qn, AcRecord i t, qn'), (qx', AcCheck i t, qx)] 
        (eargs, cfg')               = pgArgs' argctxs (i,t,q,s,v,ctx,stx,d_minus ds) 
    e -> error $ "pgArgs: not supported arg " ++ show e 


pgArgs :: [Term] -> Config -> [[FunOrArg Int]] -> DupDepth -> Edges' 
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



pgCond :: Term -> Config -> DupDepth -> Edges' 
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
tm2exp ctx (RED (TmSTO  n) [])  =   GCLL.S   (Ox (toInteger n))
tm2exp ctx (RED (TmU256 n) [])  =   Ox n
tm2exp ctx (RED (TmDATA d) [])  =   Ox (data2nat d)
tm2exp ctx (RED (TmBOP o)[x,y]) =   pgBOP o (tm2exp ctx x) (tm2exp ctx y)

    
