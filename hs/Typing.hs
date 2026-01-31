module Typing  where 

import AST 
import GCLL
import Data
import Type 
import Param
import Term 
import Bind
import Tree
import Subtyping
import Utils
import Pattern
import Data2Functor (typedDT) 

import Prelude hiding ((<$)) 





type Constraint =  [(Ty, Ty)]






typeof :: Ctx -> Term -> Ty 
typeof _ _ = Untyped


substinty :: ID -> Ty -> Ty -> Ty 
substinty x tyT tyS = case tyS of 
    TyARR tyS1 tyS2         -> TyARR (substinty x tyT tyS1) (substinty x tyT tyS2) 
    TyAPP tyS1 tyS2         -> TyAPP (substinty x tyT tyS1) (substinty x tyT tyS2) 
    TyID s                  -> case tyT of 
        TyREC _ _               -> tyT 
        _ | s == x              -> tyT
          | otherwise           -> tyS 
    _                       -> tyS 


substinconstr :: ID -> Ty -> Constraint -> Constraint 
substinconstr x tyT []               = [] 
substinconstr x tyT ((tyC1,tyC2):cs) = 
    (substinty x tyT tyC1, substinty x tyT tyC2) : substinconstr x tyT cs 


apply_constr :: Constraint -> Ty -> Ty  
apply_constr sol tyT = 
    foldl f tyT (reverse sol) where  
        f tyS (TyID x, tyC2)    = substinty x tyC2 tyS 
        f tyS _                 = tyS

occur :: ID -> Ty -> Bool 
occur x tyT = case tyT of 
    TyARR tyT1 tyT2       -> occur x tyT1 || occur x tyT2
    TyAPP tyT1 tyT2       -> occur x tyT1 || occur x tyT2   
    TyID s                -> s == x 
    _                     -> False 



unify :: Constraint -> Constraint 
unify constraint = case constraint of 
    []                                    -> [] 
    (tyT, TyREC x tyS) : cs               -> unify ((tyS, tyT) : cs) 
    (TyREC x tyS, tyT) : cs               -> unify ((tyS, tyT) : cs) 
    (TyID x, TyD d)    : cs               -> unify cs ++ [(TyID x, TyD d)] 
    (TyD d, TyID x)    : cs               -> unify cs ++ [(TyID x, TyD d)] 
    (TyID x, tyT)      : cs | tyT==TyID x -> unify cs
                            | occur x tyT -> unify (substinconstr x tyT cs) ++ [(TyID x, TyREC x tyT)] 
                            | otherwise   -> unify (substinconstr x tyT cs) ++ [(TyID x, tyT)] 
    (tyS, TyID x)      : cs | tyS==TyID x -> unify cs 
                            | occur x tyS -> unify (substinconstr x tyS cs) ++ [(TyID x, TyREC x tyS)] 
                            | otherwise   -> unify (substinconstr x tyS cs) ++ [(TyID x, tyS)] 
    (TyARR t1 t2,TyARR s1 s2) : cs        -> unify ((t1, s1):(t2, s2) : cs) 
    (TyAPP t1 t2,TyAPP s1 s2) : cs        -> unify ((t1, s1):(t2, s2) : cs) 
    (tyS,  tyT)        : cs | tyS == tyT  -> unify cs  
                            | otherwise   -> error $ "unify: Unsolvable Constraints:" ++ "(" ++ show tyS ++ "," ++ show tyT ++ ")" ++  show constraint


var :: Int -> String 
var q = "?X" ++ show q


type UVar = Int 


paramret2ty :: [Param] -> Ty -> Ty 
paramret2ty ps rety = loop (reverse ps) rety where 
    loop []             ty = ty 
    loop ((_,t1):rest)  ty = TyARR t1 ty 


paramUVar params q = case params of 
    []                      -> ([],q)
    (p,Untyped) : ps        -> (params'', q') where 
        tyP                     = TyID (var q) 
        (params',q')            = paramUVar ps (q+1) 
        params''                = (p,tyP) : params' 
    (p,tyP)     : ps        -> (params'', q') where 
        (params', q')           = paramUVar ps q 
        params''                = (p,tyP) : params' 


addParamBind ctx params = case params of 
    []                      -> ctx 
    (p,tyP) : ps            -> addParamBind ctx' ps where  
        ctx' = addBind ctx p (BindTmVAR tyP)  


apply_constr_params constr params = case params of 
    []          -> []
    (id,ty):ps  -> (id, apply_constr constr ty) : apply_constr_params constr ps 





reconCN ctx stx dtx q (CN id tops) = (CN id tops', q', constr') where 
    (tops', q', constr')    = reconTOPs ctx stx dtx q [] tops 


reconTOPs :: Ctx -> Ctx -> Ctx -> UVar -> Constraint -> [TOP] -> ([TOP], UVar, Constraint) 
reconTOPs ctx stx dtx q constr tops = case tops of 
    []                                  -> ([], q, constr) 
    SV id ty                      : ts  -> (SV id ty'' : ts', q', constr') where 
        ty''                            = apply_constr sol ty' 
        sol                             = unify constr'
        ty'                             = TyID (var q) 
        stx'                            = addBind stx id (BindTmVAR ty')
        (ts', q', constr')              = reconTOPs ctx stx' dtx (q+1) constr ts
    MT id ty ps body              : ts  -> (m' : ts', q'', constr''') where
        (body', tyR, q',constr')        = reconBODY ctx stx dtx q body
        constr''                        = constr ++ constr'
        sol                             = unify constr''
        tyR'                            = double (apply_constr sol) tyR
        m'                              = MT id tyR' ps body'  
        (ts', q'',constr''')            = reconTOPs ctx stx dtx q' constr'' ts
    DT id tys ps cs               : ts  -> (dt' : ts', q', constr') where 
        dt'                             = typedDT (DT id tys ps cs)
        DT _ (dty:ctys) ps' cs'         = dt' 
--        ctx'                            = addDConstrBind ctx cs'
        dtx'                            = addDIndBind dtx (dty:ctys) 
        (ts',q',constr')                = reconTOPs ctx stx dtx' q constr ts                              

-- #TODO 
addDConstrBind ctx [] = ctx 
addDConstrBind ctx (DConstr id [ty]: ds ) = addDConstrBind (addBind ctx id (BindTmVAR ty)) ds 

addDIndBind dtx (dty:ctys) = dtx'' where 
    TyREC id ty                 = dty 
    dtx''                       = addBind dtx' id (BindTyABB dty)
    dtx'                        = loop dtx ctys where 
        loop dtx []                 = dtx
        loop dtx (TyCON id ty:cs)   = loop (addBind dtx id (BindTmVAR ty)) cs


reconBODY :: Ctx -> Ctx -> Ctx -> UVar -> BODY -> (BODY, Ty, UVar, Constraint) 
reconBODY ctx stx dtx q (BODY p1 decls tm p2) = loop ctx stx dtx q [] decls [] where 
    loop ctx stx dtx q constr ds ds' = case ds of 
        []                          -> (body, ty, q', constr ++ constr') where
            body                        = BODY p1 (reverse ds') tm p2
            (ty, q', constr')           = recon ctx stx dtx q tm 
        SLET id    ty tm p  : ds    -> loop ctx stx' dtx q' (constr ++ constr'') ds (d':ds') where 
            BindTmVAR _ty               = getBind stx (tl id) 
            constr''                    = (_ty, ty') : constr' 
            _stx                        = addBind stx (tl id) (BindTmVAR (TyID (var q)))
            (ty', q', constr')          = recon ctx _stx dtx (q+1) tm 
            stx'                        = addBind stx (tl id) (BindTmVAR ty') 
            d'                          = SLET id ty' tm p 
        FLET id ps ty tm p  : ds    -> loop ctx' stx dtx q'' constr'' ds (d':ds') where 
            tyR                         = TyID (var q)  
            (_ps,q')                    = paramUVar ps (q+1) 
            tyF                         = paramret2ty _ps tyR
            _ctx                        = addBind ctx id (BindTmVAR tyF)
            __ctx                       = addParamBind _ctx _ps 
            (rety, q'', constr')        = recon __ctx stx dtx q' tm 
            constr''                    = [(tyR, rety)] ++ constr' ++ constr
            sol                         = unify constr'' 
            tyF'                        = (apply_constr sol) tyF 
            rety'                       = (apply_constr sol) rety 
            ps'                         = (apply_constr_params sol) _ps
            ctx'                        = addBind ctx id (BindTmVAR tyF') 
            d'                          = FLET id ps' rety' tm p
        LET  id    ty tm p  : ds    -> loop ctx' stx dtx q' (constr ++ constr') ds (d':ds') where 
            _ctx                        = addBind ctx id (BindTmVAR (TyID (var q)))
            (ty', q', constr')          = recon _ctx stx dtx (q+1) tm 
            sol                         = unify (constr') 
            ty''                        = (apply_constr sol) ty' 
            ctx'                        = addBind ctx id (BindTmVAR ty'') 
            d'                          = LET id ty'' tm p 
        
        
        


recon :: Ctx -> Ctx -> Ctx -> UVar -> Term -> (Ty, UVar, Constraint) 
recon ctx stx dtx q (RED tm trs)    = case tm of 
    TmVAR i                         ->  (tyT, q, []) where 
                                        tyT     = getTy ctx i 
    TmSTO i                         ->  (tyT, q, []) where 
                                        tyT     = getTy stx i 
    TmABS x _                       ->  (TyARR tyX tyT, q'', cnstr') where 
                                        tyX     = TyID (var q) 
                                        ctx'    = addBind ctx x (BindTmVAR tyX) 
                                        (tyT,q'',cnstr')    = recon ctx' stx dtx (q+1) (hd trs) 
    TmAPP                           ->  (TyID x, q''+1, cs ++ cs' ++ cs'') where 
                                        x                   = var q'' 
                                        cs                  = [(tyT1, TyARR tyT2 (TyID x))]
                                        (tyT1, q',cs')      = recon ctx stx dtx q  t1
                                        (tyT2, q'',cs'')    = recon ctx stx dtx q' t2
                                        [t1,t2]             = trs 
                                        {--
    TmFIX f x _                     ->  let (ty,q',cs) = recon ctx stx q (hd trs) in 
                                        case simplifyty ctx ty of 
        TyARR tyS tyT | tyT' <$ ctx $ tyS'  -> (tyT', q, cs) 
                      | otherwise           -> error "recon: TmFIX can take type A -> A" 
            where   tyT' = apply_constr sol tyT
                    tyS' = apply_constr sol tyS
                    sol  = unify ctx ((tyS,tyT) : cs )
                    --} 
    TmIF                            ->  (tyT3, q''', cs ++ cs' ++ cs'' ++ cs''') where 
                                        (tyT1, q'  , cs'  )     = recon ctx stx dtx q   tr1
                                        (tyT2, q'' , cs'' )     = recon ctx stx dtx q'  tr2
                                        (tyT3, q''', cs''')     = recon ctx stx dtx q'' tr3
                                        cs = [(tyT1,TyBOOL),(tyT2,tyT3)]
                                        [tr1, tr2, tr3] = trs 
    TmBOP o                         ->  let [tr1,tr2]           = trs 
                                            (tyT1, q' , cs' )   = recon ctx stx dtx q  tr1
                                            (tyT2, q'', cs'')   = recon ctx stx dtx q' tr2 in 
                                        case o of 
        "<"                             ->  (TyBOOL, q'', [(tyT1, tyT2)] ++ cs' ++ cs'') 
        ">"                             ->  (TyBOOL, q'', [(tyT1, tyT2)] ++ cs' ++ cs'') 
        "&&"                            ->  (TyBOOL, q'', [(tyT1, TyBOOL),(tyT2, TyBOOL)] ++ cs' ++ cs'')
        "||"                            ->  (TyBOOL, q'', [(tyT1, TyBOOL),(tyT2, TyBOOL)] ++ cs' ++ cs'')
        "=="                            ->  (TyBOOL, q'', [(tyT1, tyT2)] ++ cs' ++ cs'') 
        "!="                            ->  (TyBOOL, q'', [(tyT1, tyT2)] ++ cs' ++ cs'') 
        "<="                            ->  (TyBOOL, q'', [(tyT1, tyT2)] ++ cs' ++ cs'') 
        ">="                            ->  (TyBOOL, q'', [(tyT1, tyT2)] ++ cs' ++ cs'') 
        "+"                             ->  (tyT1  , q'', [(tyT1, tyT2)] ++ cs' ++ cs'') 
        "-"                             ->  (tyT1  , q'', [(tyT1, tyT2)] ++ cs' ++ cs'') 
        "*"                             ->  (tyT1  , q'', [(tyT1, tyT2)] ++ cs' ++ cs'') 
        "/"                             ->  (tyT1  , q'', [(tyT1, tyT2)] ++ cs' ++ cs'') 
        "%"                             ->  (tyT1  , q'', [(tyT1, tyT2)] ++ cs' ++ cs'') 
    TmDATA d                        ->  (TyNAT, q, []) 
    TmERR                           ->  (TyERR, q, []) 
    TmCON n id                      ->  (tyT, q, [])  where 
                                            tyT  = getTy dtx n 
    TmCASE                          ->  reconPATTERNs ctx stx dtx q pts tyMatch where 
                                            pts     = tl trs 
                                            match   = hd trs 
                                            (tyMatch,_,_) = recon ctx stx dtx q match 

    tm                              -> error $ "recon not defined tm : " ++ show (RED tm trs)  

tyD2tyC :: Ctx -> ID -> [(ID,Ty)] 
tyD2tyC [] id = error $ "tyD2tyC: cannot find Datatype :" ++ id  
tyD2tyC ((s,BindTyABB ty):dtx) id | id == s = loop dtx []    where 
    loop [] ret                     = ret 
    loop ((s,BindTyABB _):dtx) ret  = ret 
    loop ((s,BindTmVAR ty):dtx)ret  = loop dtx ((s,ty):ret)  
tyD2tyC (_:dtx) id = tyD2tyC dtx id                         

reconPATTERNs :: Ctx -> Ctx -> Ctx -> Int -> [Term] -> Ty -> (Ty, Int, Constraint) 
reconPATTERNs ctx stx dtx q pts ty = loop ctx q pts [] where 
    TyD id    = getTyD ty 
    constrTys = tyD2tyC dtx id  
    loop ctx q []       ret     = unifyPattern ret  
    loop ctx q (pt:pts) ret     = case pt of 
        RED (TmPATTERN (PCon id ps)) [t] -> loop ctx q pts ((tyT, q', constr):ret) where 
            cty                     =   getPConTy constrTys id 
            ctx'                    =   addBindPCon dtx ctx ps cty 
            (tyT, q', constr)       =   recon ctx' stx dtx q t 
        RED (TmPATTERN (PWild)) [t]      -> loop ctx q pts ((tyT, q', constr):ret)  where 
            (tyT, q', constr)       =   recon ctx stx dtx q t 
        RED (TmPATTERN (PVar s))[t]      -> loop ctx' q pts ((tyT, q', constr):ret) where 
            ctx'                    =   addBind ctx s (BindTmVAR ty) 
            (tyT, q', constr)       =   recon ctx' stx dtx q t 
        _                           ->  error "reconPATTERN: expected PATTERN TERM" 

getTyD (TyD id)         = TyD id 
getTyD (TyAPP tyA tyB)  = getTyD tyA 
getTyD ty               = error $ "getTyD: unexpected type " ++ show ty 


unifyPattern :: [(Ty, Int, Constraint)] -> (Ty, Int, Constraint)
unifyPattern [] = error $ "unifyPattern: no contents"
unifyPattern [ret] = ret
unifyPattern ((ty,q,constr):(ty',q',constr'):rest) = unifyPattern ((ty,q, constr ++ constr' ++ [(ty,ty')]):rest)  


addBindPCon :: Ctx -> Ctx -> [Pattern] -> Ty -> Ctx 
addBindPCon dtx ctx [] _        = ctx 
addBindPCon dtx ctx (PVar x:ps) (TyARR tyX tyA) = addBindPCon dtx (addBind ctx x (BindTmVAR tyX)) ps tyA   
addBindPCon dtx ctx (PWild :ps) (TyARR _   tyA) = addBindPCon dtx ctx ps tyA
addBindPCon dtx ctx (PCon cid cps:ps) (TyARR (TyD id) tyA) = addBindPCon dtx ctx' ps tyA where 
    ctx'    = addBindPCon dtx ctx cps cty 
    cntys   = tyD2tyC dtx id 
    cty     = getPConTy cntys cid

getPConTy :: [(ID,Ty)] -> ID -> Ty
getPConTy [] id                         = error $ "getPConTy: cannot find type of " ++ id 
getPConTy ((s,ty):rest) id  | s == id   = ty 
                            | otherwise = getPConTy rest id             

                                        


                                            


{--
data Tm     = TmAPP                 -- 2 args 
            | TmABS ID Ty           -- 1 arg
            | TmVAR Int             -- 0
            | TmSTO Int             -- 0 Storage Variable  
            | TmPROD                -- n 
            | TmFIX ID ID Ty        -- 1
            | TmU8 Int              -- 0
            | TmU256 Integer        -- 0
            | TmTRUE                -- 0
            | TmFALSE               -- 0
            | TmNOT                 -- 1
            | TmI Int Int           -- 0 
            | TmIREC  Int           -- 0 
            | TmISTR  Int           -- 0 structural recursion 
            | TmIF                  -- 3 
            | TmAMOUNT              -- 0 EVM Value
            | TmTHIS                -- 0 THIS CONTRACT ADDRESS
            | TmSENDER              -- 0 SENDER 
            | TmCALL                -- 3 (4) args { to , value , input (, gascap) } 
            | TmRET                 
            | TmBOP String      
            | TmUOP String    
            -- Declarations 
            | TmLET  ID Ty          -- 2 assignment 
            | TmSLET ID Ty          -- 2 storage assignment 
            | TmFUN  ID Ty [Param]  -- 2 Function Declaration 
            | TmMT   ID Ty [Param]  -- 2 Method   Declaration 
            | TmCN   ID 
            -- Unit Operations
            | TmSEND

            | Eff STMT
            deriving (Show, Eq, Read) 
--} 



-- RETURN Type 
--  if you want to return Function type,
--  then you have to make a contract and embed the function into it, and 
--  returns the address of contract and the method name. 
--  
--  if the returning function was an anonymous function show the error, 
--  ERROR: RETURNING FUNCTION cannot be an ANONYMOUS FUNCTION 
--

