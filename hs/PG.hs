-- Program Graph 


module PG where 

import GCLL 



data Var  = X Int deriving (Show, Eq, Read) 
data Node = Q Int deriving (Show, Eq, Read) 

--data STMT   = Stop
--            | Revert EXPR EXPR 
--            | Return EXPR EXPR 
--            | Label String
--            | Pop 
--            | Push EXPR
--            | Assign EXPR EXPR
--            | IfGoto  EXPR EXPR 
--            | Goto EXPR 
--            | Seq  [STMT] 
--            | Swap Int 
--            | Dup  Int 
--            | Calldatacopy EXPR EXPR EXPR 
--            | Codecopy     EXPR EXPR EXPR
--            | Extcodecopy
--            deriving (Eq, Read) 

data Action     = AcStop 
                | AcRevert EXPR EXPR
                | AcReturn EXPR EXPR
                | AcPop 
                | AcPush EXPR
                | AcSkip                -- correspond to GOTO 
                | AcAssgin Var EXPR
                | AcBool   EXPR         -- correspond to IFGOTO
                | AcEnter               -- make empty frame 
                | AcExit                -- Return stacktop, remove frame, push stacktop
                | AcVar Var
                | AcArray Var Int
                | AcRecord Node Node    -- 
                | AcCheck Node Node 
                

