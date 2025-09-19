module Typing  {-# WARNING "statically type checking NOT implemented now" #-} where 

import GCLL
import Type 
import Term 
import Tree


typeof (BLK _ _) = Untyped

typeof (RED tm trs) = case tm of 
    TmU8  i             -> TyU8
    TmU256 i            -> TyU256
    _                   -> Untyped 



substinconstr = undefined 

occur = undefined 


unify ctx constraint = case constraint of 
    (tyT, TyREC x tyS) : cs                 -> unify ctx ((tyS, tyT) : cs) 
    (TyREC x tyS, tyT) : cs                 -> unify ctx ((tyS, tyT) : cs) 
    (TyVAR x, tyT)     : cs | tyT==TyVAR x  -> unify ctx cs
                            | occur x tyT   -> unify ctx (substinconstr x tyT cs) 
                                            ++ [(TyVAR x, TyREC x tyT)] 
                            | otherwise     -> unify ctx (substinconstr x tyT cs) 
                                            ++ [(TyVAR x, tyT)] 
    (tyS, TyVAR x)     : cs | tyS==TyVAR x  -> unify ctx cs 
                            | occur x tyS   -> unify ctx (substinconstr x tyS cs)
                                            ++ [(TyVAR x, TyREC x tyS)] 
                            | otherwise     -> unify ctx (substinconstr x tyS cs)
                                            ++ [(TyVAR x, tyS)] 
    (TyMAP t1 t2,TyMAP s1 s2) : cs          -> unify ctx ((t1, s1):(t2, s2) : cs) 
    (tyS,  tyT)        : cs | tyS == tyT    -> unify ctx cs  
                            | otherwise     -> error "unify: Unsolvable Constraints" 
    _                                       -> error "unify: NoRuleApplies" 
                                            


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

