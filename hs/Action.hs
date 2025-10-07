module Action where 

import GCLL
import Node
import Edge 
import Semiring 


data Var  = X Int deriving (Show, Eq, Read) 
data Sto  = S Int deriving (Show, Eq, Read) 


data Action     = AcStop 
                | AcCond 
                | AcDonc 
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
                | AcExtcodecopy EXPR EXPR EXPR EXPR
                | AcSkip                -- correspond to GOTO 
                | AcSstore 
                | AcMstore 
                -- | AcAssign Var EXPR
                -- | AcSassign Sto EXPR    
                | AcBool   EXPR         -- correspond to IFGOTO
                | AcElse                -- Does this  
                | AcEnter               -- make empty frame 
                | AcExit                -- Return stacktop, remove frame, push stacktop
                | AcVar Int
                | AcSto Int
                | AcArray Var Int
                | AcRecord (Node Int) (Node Int)
                | AcCheck  (Node Int) (Node Int)     
                | AcSeq [Action] 
                deriving (Eq, Read)

instance Ord Action where 
    _ <= AcCond             = True 
    _ <= AcDispatch _       = True 
    _ <= AcCheck _ _        = True 
    AcCond <= _             = False
    AcDispatch _ <= _       = False
    AcCheck _ _ <= _        = False 
    _ <= _                  = True
    

                
instance Show Action where 
    show (AcStop                ) = "STOP"
    show (AcDispatch s          ) = "DSP " ++  s 
    show (AcRevert e f          ) = "RV" ++ show [e,f] 
    show (AcReturn e f          ) = "RT" ++ show [e,f] 
    show (AcPop                 ) = "POP "
    show (AcPush e              ) = show e 
    show (AcSwap i              ) = "SWP" ++ show i 
    show (AcDup  i              ) = "DUP" ++ show i 
    show (AcBop  s              ) = s 
    show (AcCond                ) = "{" 
    show (AcDonc                ) = "}" 
    show (AcCalldatacopy e f g  ) = "CALLDTCP" ++ show [e,f,g] 
    show (AcCodecopy     e f g  ) = "CDCP"     ++ show [e,f,g] 
    show (AcExtcodecopy e f g h ) = "EXCDCP" ++ show [e,f,g,h] 
    show (AcSkip                ) = "SKIP"
    show (AcSstore              ) = "SSTORE"
    show (AcMstore              ) = "MSTORE" 
    -- show (AcAssign x a          ) = show x ++ ":=" ++ show a 
    -- show (AcSassign s a         ) = show s ++ ":=" ++ show a 
    show (AcBool e              ) = show e -- ++ "?" 
    show (AcElse                ) = "ELSE" 
    show (AcEnter               ) = "ENTR"
    show (AcExit                ) = "EXIT" 
    show (AcVar i               ) = "VAR" ++ show i 
    show (AcSto i               ) = "STO" ++ show i 
    show (AcArray a i           ) = show a ++ show [i]
    show (AcRecord (Q i)(Q j)   ) = "RC" ++ show i ++ "/" ++ show j
    show (AcCheck  (Q i)(Q j)   ) = "CK" ++ show i ++ "/" ++ show j


isCnd :: Action -> Bool 
isCnd (AcBool _)        = True 
isCnd (AcCond)          = True
isCnd _                 = False 

isChk (AcCheck _ _)     = True
isChk _                 = False 

isDsp (AcDispatch s)    = True 
isDsp _                 = False 

isSkp (AcSkip)          = True   
isSkp _                 = False 



--instance Semiring [Action] where 
--    zero    = [] 
--    one     = [AcSkip] 
--    a <+> b = a ++ b  
--    a <.> b = [AcSeq(a ++ b)] 
--
--instance {-# Overlapping #-} Semigroup(Maybe Action) where 
--   (<>) = (<.>)
--instance {-# Overlapping #-} Monoid   (Maybe Action) where 
--    mempty = one 
--instance Semiring (Maybe Action) where 
--    zero                = Nothing 
--    one                 = Just AcSkip
--    Nothing <+> b       = b 
--    a       <+> Nothing = a 
--    a       <+> b       = undefined 
--    Just a  <.> Just b  = Just (AcSeq[a,b]) 
--    _       <.> _       = Nothing 
