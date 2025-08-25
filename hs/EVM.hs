module EVM where 



import GCLL

import Data.Map.Strict 
import Data.Word

-- Exec Env I 

data I  = I_a   -- Account
        | I_o   -- Originator
        | I_p   -- Price of gas 
        | I_d   -- Input Data
        | I_s   -- Sender
        | I_v   -- Value
        | I_b   -- Byte Code of Initialization
        | I_e   -- dEpth of Call/Create
        | I_w   -- The permission to make modification to state
             





data EVM = EVM  { stk  :: [EXPR] 
                , mem  :: Map EXPR EXPR  
                , stor :: Map EXPR EXPR
                , code :: [Word8] 
                } deriving (Show) 


emptyEVM = EVM [] empty empty 




