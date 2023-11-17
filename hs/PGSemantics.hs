module PGSemantics where 

import PG
import Configure







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
                | AcRecord Nd Nd
                | AcCheck  Nd Nd    
                -- | AcSeq [Action] 
                deriving (Eq, Read) 
                
instance Show Action where 
    show (AcStop                ) = "STOP"
    show (AcDispatch s          ) = s 
    show (AcRevert e f          ) = "RV" ++ show [e,f] 
