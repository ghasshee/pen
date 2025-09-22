module Typing  where 


import GCLL
import Type 
import Term 
import Bind
import Tree
import Subtyping

import Prelude hiding ((<$)) 





type Constraint =  [(Ty, Ty)]


hd :: [a] -> a  
hd []       = error "hd: empty list" 
hd (a:_)    = a 





typeof :: Ctx -> Term -> Ty 
typeof ctx (BLK _ _) = Untyped
typeof ctx (RED tm trs) = case tm of 
    TmU8  i             -> TyU8
    TmU256 i            -> TyU256
    _                   -> Untyped 



substinty :: ID -> Ty -> Ty -> Ty 
substinty x tyT tyS = case tyS of 
    TyARR tyS1 tyS2         -> TyARR (substinty x tyT tyS1) (substinty x tyT tyS2) 
    TyID s                  -> case tyT of 
        TyREC _ _               -> tyT 
        _ | s == x              -> tyT
          | otherwise           -> tyS 
    _                       -> tyS 


substinconstr :: ID -> Ty -> Constraint -> Constraint 
substinconstr x tyT = ((substinty x tyT <$>) <$>)


apply_constr :: Constraint -> Ty -> Ty  
apply_constr sol tyT = 
    foldl f tyT (reverse sol) where  
        f tyS (TyID x, tyC2)   = substinty x tyC2 tyS 
        f tyS _                 = tyS

occur :: ID -> Ty -> Bool 
occur x tyT = case tyT of 
    TyARR tyT1 tyT2        -> occur x tyT1 || occur x tyT2
    TyID s                -> s == x 
    _                      -> False 



unify :: Ctx -> Constraint -> Constraint 
unify ctx constraint = case constraint of 
    []                                      -> [] 
    (tyT, TyREC x tyS) : cs                 -> unify ctx ((tyS, tyT) : cs) 
    (TyREC x tyS, tyT) : cs                 -> unify ctx ((tyS, tyT) : cs) 
    (TyID x, tyT)     : cs | tyT==TyID x  -> unify ctx cs
                            | occur x tyT   -> unify ctx (substinconstr x tyT cs) 
                                            ++ [(TyID x, TyREC x tyT)] 
                            | otherwise     -> unify ctx (substinconstr x tyT cs) 
                                            ++ [(TyID x, tyT)] 
    (tyS, TyID x)     : cs | tyS==TyID x  -> unify ctx cs 
                            | occur x tyS   -> unify ctx (substinconstr x tyS cs)
                                            ++ [(TyID x, TyREC x tyS)] 
                            | otherwise     -> unify ctx (substinconstr x tyS cs)
                                            ++ [(TyID x, tyS)] 
    (TyARR t1 t2,TyARR s1 s2) : cs          -> unify ctx ((t1, s1):(t2, s2) : cs) 
    (tyS,  tyT)        : cs | tyS == tyT    -> unify ctx cs  
                            | otherwise     -> error "unify: Unsolvable Constraints" 
    cs                                      -> error $ "unify: NoRuleApplies: " ++ show cs 


var :: Int -> String 
var q = "?X" ++ show q









reconTOPs ctx q tops = undefined 

reconDecls ctx q decl = undefined 



recon ctx q (RED tm trs)    = case tm of 
    TmVAR i                         ->  (tyT, q, []) where 
                                        tyT     = getTy ctx i 
    TmABS x _                       ->  (TyARR tyX tyT, q'', cnstr') where 
                                        tyX     = TyID (var q) 
                                        ctx'    = addBind ctx x (BindTmVAR tyX) 
                                        (tyT,q'',cnstr')    = recon ctx' (q+1) (hd trs) 
    TmAPP                           ->  (TyID x, q''+1, cs ++ cs' ++ cs'') where 
                                        x                   = var q'' 
                                        cs                  = [(tyT1, TyARR tyT2 (TyID x))]
                                        (tyT1, q',cs')      = recon ctx q  t1
                                        (tyT2, q'',cs'')    = recon ctx q' t2
                                        [t1,t2]             = trs 
                                        {--
    TmFIX f x _                     ->  let (ty,q',cs) = recon ctx q (hd trs) in 
                                        case simplifyty ctx ty of 
        TyARR tyS tyT | tyT' <$ ctx $ tyS'  -> (tyT', q, cs) 
                      | otherwise           -> error "recon: TmFIX can take type A -> A" 
            where   tyT' = apply_constr sol tyT
                    tyS' = apply_constr sol tyS
                    sol  = unify ctx ((tyS,tyT) : cs )
                    --} 
    TmIF                            ->  (tyT3, q''', cs ++ cs' ++ cs'' ++ cs''') where 
                                        (tyT1, q'  , cs'  )     = recon ctx q   tr1
                                        (tyT2, q'' , cs'' )     = recon ctx q'  tr2
                                        (tyT3, q''', cs''')     = recon ctx q'' tr3
                                        cs = [(tyT1,TyBOOL),(tyT2,tyT3)]
                                        [tr1, tr2, tr3] = trs 
    TmBOP o                         ->  let [tr1,tr2]           = trs 
                                            (tyT1, q' , cs' )   = recon ctx q  tr1
                                            (tyT2, q'', cs'')   = recon ctx q' tr2 in 
                                        case o of 
        "<"                             ->  (TyBOOL, q'', [(tyT1, tyT2)] ++ cs' ++ cs'') 
        ">"                             ->  (TyBOOL, q'', [(tyT1, tyT2)] ++ cs' ++ cs'') 
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
    tm      -> error $ "recon not defined tm : " ++ show (RED tm trs)  
    
                    
                                    
                                        


                                            


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

