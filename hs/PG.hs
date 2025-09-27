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
pg = pgCN initialPG  



-------------------------
--    Program Graph    --  
-------------------------


pgCN pg (CN id tops) = pgTOPs pg tops 

pgTOPs                                 =   foldl pgTOP 
pgTOP (es,(i,t,q,s,v,ctx,stx))  top    =   case top of 
    (SV id ty)          ->  (es ++ e, cfg') where 
        e       = [(i,AcPush(Ox 0),Q q),(Q q,AcPush(Ox(to s)),Q(q+1)),(Q(q+1), AcSstore, Q(q+2))]
        cfg'    = (Q(q+2),t,q+3,s+1,v,ctx,id:stx)
    (MT id ty ps bd)    ->  (es'', (i,t,q',s',v',ctx',stx)) where  -- (i, t)  must NOT change 
        id'                             =   id  ++  showTyParams ps 
        es'                             =   es ++ [(i,AcDispatch id', Q q), (Q(q+1), AcSkip, t) ]  
        (es'',(_,_,q',s',v',ctx',_))    =   pgMT (es',(Q q,Q(q+1),q+2,s,v,ctx,stx)) (MT id ty ps bd)
    
pgMT   pg (MT id ty ps bd)             =   pgBODY (pgParams pg ps) bd 

pgParams                               =   foldl pgParam
pgParam (es,(i,t,q,s,v,ctx,stx)) _     =   (es ++ [(i,AcVar v,Q q)],(Q q,t,q+1,s,v+1,ctx,stx))

pgBODY (es,cfg) (BODY _ ds tm _)       =   (es'', (i',t',q',s',v',ctx',stx'))  
                                where   (es' ,  (i,t,q,s,v,ctx,stx))          = pgDecls (es,cfg) ds  
                                        (es'',  (i',t',q',s',v',ctx',stx',_)) = pgTerm (es', (i,t,q,s,v,[]:ctx,stx,[])) tm 

pgDecls                                = foldl pgDecl
pgDecl (es, cfg@(i,t,q,s,v,ctx,stx)) d = case d of 
    LET id ty tm fm         ->  pgDecl (es,cfg) (FLET id [] ty tm fm)  
    SLET id  _  tm _        ->  (es' ++ e ,  cfg'  ) where   
        (es', (i',t',q',s',v',ctx',stx',_))         = pgTerm (es, (i,Q q,q+3,s,v,[]:ctx,id:stx,[])) tm  
        e                                           = [(Q q, AcPush (Ox (to n)), Q(q+1)), (Q(q+1),AcSstore,Q(q+2))] 
        cfg'                                        = (Q(q+2),t,q',s',v',ctx,stx')
        Just _n                                     = searchSto id stx'  
        Just n                                      = searchSto (drop 1 id) stx' 
    FLET id ps _ tm _       ->  (es',   (i,t,q''',s''',v''',ctx'',stx'''))  where  -- (i,t) must NOT change
        arities                                     = paramArities  ps       
        cfg'@(_,_,_,_,_,ctx',_)                     = pgFunCtxs cfg  [Fun (id,len ps)] 
        Fun (_,qn,qx)                               = searchFun 0 ctx' 
        (_ ,_ ,q'',s'',v'',ctx'',stx'')             = pgFunCtxs cfg' (Arg <$> arities) 
        (es',(_,_,q''',s''',v''',ctx''',stx''',_))  = pgTerm (es, (qn,qx,q'',s'',v'',ctx'',stx'',[])) tm 


pgTerm :: PG' -> Term -> PG' 
pgTerm (es,cfg@(i,t,q,s,v,ctx,stx,ds)) tr = 
    let d       = dup_sum ds                in 
    let ds'     = d_minus ds                in 
    let cfg'    = (i,t,q,s,v,ctx,stx,ds')   in 
    let cs:css  = ctx                       in 
    let stx'    = reduced stx               in 
    case tr of 
    RED TmAPP [t1,t2]                   ->  pgTermApp (es, (i,t,q,s,v,ctx,stx,d_push 2 ds)) tr [] 
    RED (TmU256 n) []                   ->  (es ++ [(i, AcPush (Ox n           ), t)], cfg') 
    RED (TmDATA d) []                   ->  (es ++ [(i, AcPush (Ox (data2nat d)), t)], cfg') 
    RED (TmVAR  n) []                   ->  case searchFun (n+d) ctx of 
        Arg (0, qn, qx)                     -> (es ++ [(i, AcDup n',    t)], cfg') where 
            n'                                      = calc_arglen ctx (n+d) - n 
            cfg'                                    = (i,t,q,s,v,ctx',stx,d_minus (d_dup ds))  
            ctx'                                    = (Arg (0,qn,qx) : cs) : css  -- Because of !! DUP !! opearation the context gains 
        Arg (_, qn, qx)                     -> err "Dynamical Higher Order Function (Function Closure as Value) is prohibited."
        Fun (_, qn, qx)                     -> (es ++ [(i, AcSkip, qn), (qx, AcSkip, t)], cfg') 
    RED TmIF [b,t1,t2]                  ->  pgTerm (es'', (Q(q+1),t, q'',s'',v'',ctx'', stx'',ds'')) t2 where 
        (es',(_,_,q' ,s' ,v' ,ctx' ,stx' ,ds' ))        = pgCond (es,   (i,Q q,q+2,s,v,ctx,stx,d_push 3 ds)) b
        (es'',(_,_,q'',s'',v'',ctx'',stx'',ds''))       = pgTerm  (es'++enotb, (Q q,t, q' ,s',v',ctx', stx',ds')) t1
        enotb                                           = [(i, AcSkip, Q(q+1))]    
    RED (TmBOP o)[t1,t2]                ->  (es'' ++ [(Q(q+1),AcBop o,t)], cfg'')  where 
        (es',(i',t',q',s',v',ctx',stx',ds''))       = pgTerm (es ,(i,Q q,q+2,s,v,ctx,stx,d_push 2 ds)) t1
        (es'',cfg'')                                = pgTerm (es',(Q q,Q(q+1),q',s',v',ctx',stx',ds'')) t2 
    RED (TmSTO n) [] | n >= len stx'    ->  (es ++ [(i, AcSto (len stx            - n - 1), t)], cfg') 
    RED (TmSTO n) [] | stx /= stx'      ->  (es ++ [(i, AcSto (len stx - len stx' - n - 1), t)], cfg')  
    RED (TmSTO n) []                    ->  (es ++ [(i, AcSto (len stx            - n - 1), t)], cfg')  
    _                                   ->  (es                                                , cfg') 


{-- Function Execution (or Function Call ) 
 - 1. push Arguments and PUSH "Record A"  
 - 2. goto Function Init Node 
 - 3. Function Execution
 - 4. if the top element of the Record/check stack is "Record A" then exit Function Terminal Node. This process is named "CHECK A".
 - 5. exit. --} 


pgTermApp :: PG' -> Term -> [Term] -> PG' 
pgTermApp (es,cfg@(i,t,q,s,v,ctx,stx,ds)) (RED TmAPP [t1,t2]) cont = case t1 of 
    RED (TmVAR n) []    ->  let d   = dup_sum ds in 
                            let ds' = d_minus ds in 
                            case searchFun (n+d) ctx of 
        Fun(0,_,_)          -> error $ "pgTermApp: illegal TmVAR " ++ show (n+d) ++ ":Fun " ++ show ctx
        Arg(0,_,_)          -> error $ "pgTermApp: illegal TmVAR " ++ show (n+d) ++ ":Arg " ++ show ctx
        Arg(_,qn,qx){--f@f(f x)--}  -> pgArgs (es++eent++ercd++echk++eexit,(Q q,Q(q+1),q+3,s,v,ctx,stx,ds')) (t2:cont) where 
            eent                =   [(i     , AcEnter     , Q q     )] 
            ercd                =   [(Q(q+1), AcRecord i t, qn      )]
            echk                =   [(qx    , AcCheck  i t, Q(q+2)  )] 
            eexit               =   [(Q(q+2), AcExit      , t       )] 
        Fun(_,qn,qx){-- double --}  ->  pgArgs' (es++eent++ercd++echk++eexit,(Q q,Q(q+1),q+3,s,v,ctx,stx,ds')) argctxs where 
            args                =   t2 : cont 
            actx                =   searchArgs (n+d) ctx 
            argctxs             =   zip args actx 
            eent                =   [(i     , AcEnter     , Q q     )] 
            ercd                =   [(Q(q+1), AcRecord i t, qn      )] 
            echk                =   [(qx    , AcCheck  i t, Q(q+2)  )]  
            eexit               =   [(Q(q+2), AcExit      , t       )] 
    _                   -> pgTermApp (es,cfg)  t1 (t2:cont)


no_varg :: [(Term,Var)] -> Bool 
no_varg []                  = True 
no_varg ((_,Arg(0,_,_)):_ ) = False  
no_varg ((_,Arg(_,_,_)):cs) = no_varg cs

var_out_of_ctx :: VarCtx -> Int -> Bool 
var_out_of_ctx ctx n = n >= len (hd ctx) 


pgArgs' :: PG' -> [(Term,Var)] -> PG' 
pgArgs' pg []                                                      = pg 
pgArgs' (es,cfg@(i,t,q,s,v,ctx@(cs:css),stx,ds)) (argctx:argctxs)  =  
    let d           = dup_sum ds                 in 
    let ds'         = d_dup ds                   in  
    let (_ds,_ds')  = (d_minus ds,d_minus ds')   in 
    case argctx of 
    (RED(TmVAR n)[], Arg(0,_,_)) | var_out_of_ctx ctx (n+d) -> (es ++ e,cfg) where 
        Fun(0,qn,qx)                =  searchFun (n+d) ctx 
        e                           =  [(i,AcSkip,qn), (qx,AcSkip,t)] 
        cfg                         =  (i,t,q,s,v,ctx,stx,d_minus ds)
    (RED(TmVAR n)[], Arg(0,qn,qx)) | no_varg argctxs  -> pgArgs' (es ++ earg, cfg') argctxs where 
        earg                        = [(i, AcDup n',    t)]
        n'                          = calc_arglen ctx (n+d) - n 
        cfg'                        = (i,t,q,s,v,ctx',stx,_ds')  
        ctx'                        = (Arg (0,qn,qx) : cs) : css  -- Since DUP_ gains the stack, the context gains 
    (RED(TmVAR n)[], Arg(0,qn,qx))                    -> pgArgs' (es ++ earg, cfg') argctxs where 
        earg                        = [(i, AcDup n',  Q q)]
        n'                          = calc_arglen ctx (n+d)- n 
        cfg'                        = (Q q,t,q+1,s,v,ctx',stx,_ds')  
        ctx'                        = (Arg (0,qn,qx) : cs) : css  -- Since DUP_ gains the stack, the context gains 
    (tm, Arg(0,qn,qx)) | no_varg argctxs  ->  pgArgs' (pgTerm (es,cfg) tm) argctxs 
    (tm, Arg(0,qn,qx))                    ->  pgArgs' (es',(Q q,t,q',s',v',ctx',stx',ds')) argctxs where 
        (es',(_,_,q',s',v',ctx',stx',ds'))  = pgTerm  (es,(i,Q q,q+1,s,v,ctx,stx,ds)) tm  
    (RED(TmVAR n)[], Arg(l,qn,qx)) {-- Higher Order Arg --} -> pgArgs' (es ++ earg,(i,t,q,s,v,ctx,stx,_ds)) argctxs where 
        d                           = dup_sum ds 
        Fun(_,qn',qx')              = searchFun (n+d) ctx   
        earg                        = [(qn, AcRecord i t, qn'), (qx', AcCheck i t, qx)] 
    e -> error $ "pgArgs: not supported arg " ++ show e 


pgArgs :: PG' -> [Term] -> PG' 
pgArgs pg []                                    =   pg  
pgArgs pg [tm]                                  =   pgTerm pg tm 
pgArgs (es,cfg@(i,t,q,s,v,ctx,stx,ds))(tm:tms)  =   pgArgs (es', (Q q,  t, q' ,s',v',ctx',stx',ds')) tms where 
    (es',(_,_,q',s',v',ctx',stx',ds'))          =   pgTerm (es,  (i , Q q, q+1,s ,v ,ctx ,stx ,ds )) tm              


-- possibility 
-- 1. in function defintion,  variable call
-- 2. in function defintion,  function 
-- 3. in function call     ,  defined function variable 
-- 4. in function call     ,  just value 


_pgCond :: PG' -> Term -> PG' 
_pgCond (es, cfg@(i,t,q,s,v,ctx,stx,ds)) (RED (TmBOP op) [t1,t2]) = 
    (es ++ [(i,AcBool ((pgBOP op) t1' t2'), t)], (i,t,q,s,v,ctx,stx,d_minus ds)) where 
        t1' = tm2exp ctx t1
        t2' = tm2exp ctx t2 

pgCond :: PG' -> Term -> PG' 
pgCond (es, cfg@(i,t,q,s,v,ctx,stx,ds)) tm = (es ++ econd' , (i,t,q',s',v',ctx,stx,ds)) where 
    econd'                                    = [(i,AcCond (arrow <$> econd),t)] 
    (econd,(i',t',q',s',v',ctx',stx',ds'))    = pgTerm ([], (Q q, t,q,s,v,ctx,stx,ds)) tm 


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
tm2exp ctx (RED (TmVAR  n) [])  =   Var (show n)  
tm2exp ctx (RED (TmSTO  n) [])  =   GCLL.S (Ox (to n))
tm2exp ctx (RED (TmU256 n) [])  =   Ox n
tm2exp ctx (RED (TmU8   n) [])  =   Ox (to n)
tm2exp ctx (RED (TmDATA d) [])  =   Ox (data2nat d)
tm2exp ctx (RED (TmBOP o)[x,y]) =   pgBOP o (tm2exp ctx x) (tm2exp ctx y)

    
