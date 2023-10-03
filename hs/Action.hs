module Action where 

import GCLL


data Var  = X Int deriving (Show, Eq, Read) 
data Sto  = S Int deriving (Show, Eq, Read) 
data Node = Q Int 
          | Qi | Qt | Qe
                deriving (Show, Eq, Read) 

data Action     = AcStop 
                | AcDispatch String 
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
                | AcSkip                -- correspond to GOTO 
                | AcAssign Var EXPR
                | AcBool   EXPR         -- correspond to IFGOTO
                | AcElse                -- Does this  
                | AcEnter               -- make empty frame 
                | AcExit                -- Return stacktop, remove frame, push stacktop
                | AcVar Int
                | AcSto Int
                | AcArray Var Int
                | AcRecord Node Node    -- 
                | AcCheck Node Node 
                -- | AcSeq [Action] 
                deriving (Eq, Read) 
                
instance Show Action where 
    show (AcStop                ) = "STOP"
    show (AcDispatch s          ) = s 
    show (AcRevert e f          ) = "RV" ++ show [e,f] 
    show (AcReturn e f          ) = "RT" ++ show [e,f] 
    show (AcPop                 ) = "POP "
    show (AcPush e              ) = show e 
    show (AcSwap i              ) = "SWP" ++ show i 
    show (AcDup  i              ) = "DUP" ++ show i 
    show (AcBop  s              ) = s 
    show (AcCalldatacopy e f g  ) = "CALLDTCP" ++ show [e,f,g] 
    show (AcCodecopy     e f g  ) = "CDCP"     ++ show [e,f,g] 
    show (AcExtcodecopy         ) = "EXCDCP"   
    show (AcSkip                ) = "SKIP"
    show (AcAssign x a          ) = show x ++ ":=" ++ show a 
    show (AcBool e              ) = show e -- ++ "?" 
    show (AcElse                ) = "ELSE" 
    show (AcEnter               ) = "ENTR"
    show (AcExit                ) = "EXIT" 
    show (AcVar i               ) = "VAR" ++ show i 
    show (AcSto i               ) = "STO" ++ show i 
    show (AcArray a i           ) = show a ++ show [i]
    show (AcRecord Qi Qt        ) = "RC" ++ "i/t"
    show (AcRecord Qi(Q j)      ) = "RC" ++ "i/" ++ show j
    show (AcRecord (Q i)Qt      ) = "RC" ++ show i ++ "/t"
    show (AcRecord (Q i)(Q j)   ) = "RC" ++ show i ++ "/" ++ show j
    show (AcCheck  Qi Qt        ) = "CK" ++ "i/t"
    show (AcCheck  Qi(Q j)      ) = "CK" ++ "i/" ++ show j
    show (AcCheck  (Q i)Qt      ) = "CK" ++ show i ++ "/t"
    show (AcCheck  (Q i)(Q j)   ) = "CK" ++ show i ++ "/" ++ show j


data Transition = Tr (Int,Action,Int) deriving (Eq) 

instance Show Transition where 
    show (Tr (i,a,j))         = show i ++ "=" ++ show a ++ "=>" ++ show j 


tr2ac :: Transition -> Action
tr2ac (Tr (_,a,_)) = a 
