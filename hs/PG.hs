-- Program Graph 


module PG where 

import GCLL 



data Var  = X Int deriving (Show, Eq, Read) 
data Node = Q Int deriving (Show, Eq, Read) 

data Action     = AcStop 
                | AcRevert EXPR EXPR
                | AcReturn EXPR EXPR
                | AcPop 
                | AcPush EXPR
                | AcSwap Int 
                | AcDup  Int 
                | AcCalldatacopy EXPR EXPR EXPR 
                | AcCodecopy EXPR EXPR EXPR
                | AcExtcodecopy 
                -- | AcSeq [Action] 
                | AcSkip                -- correspond to GOTO 
                | AcAssgin Var EXPR
                | AcBool   EXPR         -- correspond to IFGOTO
                | AcEnter               -- make empty frame 
                | AcExit                -- Return stacktop, remove frame, push stacktop
                | AcVar Var
                | AcArray Var Int
                | AcRecord Node Node    -- 
                | AcCheck Node Node 
                





