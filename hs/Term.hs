module Term where 

import Tree
import Type
import GCLL 
import Data


data K = K (Term -> Term)   

instance Show K where 
    show(K a) = "Kont" 

instance Eq K where 
    _ == _ = False   

instance Read K where 
    readsPrec _ = undefined 

type Bind   = (ID, Ty) 
type Param  = (ID, Ty) 

data Tm     = TmAPP                 -- 2 args 
            | TmABS ID Ty           -- 2 function body & predicate 
            | TmSSTORE              -- 4 stored variable & storing value & predicate & continuation   
            | TmFIX ID [ID] Ty      -- 2 function body & predicate 
            | TmVAR Int             -- 0
            | TmSTO Int             -- 0 Storage Variable  
            | TmPROD                -- n 
            | TmLAM ID Ty 
            | TmDATA Data
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
            | TmFLET ID Ty [Param]  -- 2 Function Declaration 
            | TmMT   ID Ty [Param]  -- 2 Method   Declaration 
            | TmCN   ID 
            -- Unit Operations
            | TmSEND                -- really sending amount
            | TmTRANSFER            -- just change the balance table contracts have
            | TmKont K 
            | TmFIXK ID [ID] K Ty 
            | Predicate Formulae

            | Eff STMT
            deriving (Show, Eq, Read) 



type Term    = RBTree Tm 


show' :: Term -> String 
show' (RED (TmSTO  i)   _   )   = "S[" ++ show i ++ "]"  
show' (RED (TmVAR  i)   _   )   = "X" ++ show i  
show' (RED (TmU256 i)   _   )   = show i 
show' (RED (TmDATA d)   _   )   = show d 
show' (RED  a           []  )   = show a 
show' (RED (TmABS x ty)  [a])   = "λ" ++ x ++ "." ++ show' a 
show' (RED (TmFIX f xs ty)[a])  = "(fix " ++ f ++ " " ++ showArgs xs ++ "." ++ show' a ++ ")" 
show' (RED (TmBOP o)    [a,b])  = show' a ++ o ++ show' b 
show' (RED (TmUOP o)    [a])    = o ++ show' a
show' (RED TmIF         [a,b,c])= "if" ++ show' a ++ "then" ++ show' b ++ "else" ++ show' c
show' (RED TmCALL       [a,b,c])= "call(" ++ show' a ++ ", " ++ show' b ++ ", " ++ show' c ++ ")" 
show' a                         = show a 

showArgs [] = ""
showArgs [x] = "x"
showArgs (x:xs) = x ++ " " ++ showArgs xs 
-- RETURN Type 
--  if you want to return Function type,
--  then you have to make a contract and embed the function into it, and 
--  returns the address of contract and the method name. 
--  
--  if the returning function was an anonymous function show the error, 
--  ERROR: RETURNING FUNCTION cannot be an ANONYMOUS FUNCTION 
--





























type Formulae = STFormulae 

-- state formulae 
data STFormulae = FTrue
                | FARROW STFormulae STFormulae 
                | FAtom AFormulae 
                | FAnd STFormulae STFormulae
                | FNot STFormulae 
                | FE PathFormulae 
                | FA PathFormulae
                deriving (Eq, Read) 

instance Show STFormulae where 
    show (FAtom   a )   = show a 
    show (FTrue     )   = "⊤"   
    show (FAnd a b  )   = show a ++ "∧" ++ show b
    show (FNot a    )   = "¬" ++ show a 
    show (FE pa      )   = "E(" ++ show pa ++ ")"
    show (FA pa      )   = "A(" ++ show pa ++ ")" 


-- path formulae 
data PathFormulae 
                = FX STFormulae
                | FF STFormulae
                | FG STFormulae
                | FU STFormulae STFormulae 
                deriving (Eq, Read) 

instance Show PathFormulae where 
    show (FX a       )   = "X(" ++ show a ++ ")"
    show (FF a       )   = "F(" ++ show a ++ ")" 
    show (FG a       )   = "G(" ++ show a ++ ")" 
    show (FU a b     )   = show a ++ "∪" ++ show b

-- atomic formulae 
data AFormulae  = AEq Term Term
                | AGt Term Term
                | ALt Term Term 
                | AGe Term Term 
                | ALe Term Term 
                deriving (Eq, Read) 
                
instance Show AFormulae where 
    show (AEq a b   )   = show' a ++ "==" ++ show' b
    show (AGt a b   )   = show' a ++ ">"  ++ show' b 
    show (ALt a b   )   = show' a ++ "<"  ++ show' b 
    show (AGe a b   )   = show' a ++ ">=" ++ show' b 
    show (ALe a b   )   = show' a ++ "<=" ++ show' b 
