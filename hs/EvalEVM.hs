module EvalEVM where 

import EVM
import Opcode
import GCLL

import Prelude hiding (EQ, LT, GT) 


import Data.Map.Strict as Map 


get i [] = error "get: empty list" 
get 1 (a:as) = a 
get n (a:as) = get (n-1) as 

remove i [] = error "remove: empty list"
remove 1 (a:as) = as 
remove n (a:as) = a : remove (n-1) as 


evalSTMT1 :: STMT -> EVM -> EVM 
evalSTMT1 s evm = case s of 
    Stop                -> evm  
    Revert e1 e2        -> evm 
    Return e1 e2        -> evm 
    Push e              -> evm { stk = e : stk evm } 
    Assign a e          -> evm { mem = Map.insert a e (mem evm) }  
    Label s             -> evm 
    Pop                 -> evm { stk = tail ( stk evm ) } 
    IfGoto b e          -> evm 
    Goto e              -> evm  
    Dup i               -> evm { stk = get i s : s } where s = stk evm 
    Swap i              -> evm { stk = get (i+1) s : remove i s } where s = stk evm 
    Codecopy t f s      -> evm -- { mem = foldn s (\i -> Map.insert (t+i) (getcode (f+i) evm)) (mem evm) }

evalSTMT :: EVM -> [STMT] -> EVM 
evalSTMT evm []         = evm
evalSTMT evm (s:ss)     = evalSTMT (evalSTMT1 s evm) ss 


evalEXPR s = case s of 
    Ox i                -> i 
    Add a b             -> evalEXPR a + evalEXPR b 
    Sub a b             -> evalEXPR a - evalEXPR b 
    Mul a b             -> evalEXPR a * evalEXPR b 
    Div a b             -> evalEXPR a `div` evalEXPR b 


foldn 0 f a = f 0 a 
foldn n f a = foldn (n-1) f (f n a) 

getcode i evm = Ox $ toInteger $ get i (code evm) 



{-- 
data STMT   = Stop
            | Revert EXPR EXPR 
            | Return EXPR EXPR 
            | Label String
            | Pop 
            | Push EXPR
            | Assign EXPR EXPR
            | IfGoto  EXPR EXPR 
            | Goto EXPR 
            | Seq  [STMT] 
            | Swap Int 
            | Dup  Int 
            | Calldatacopy EXPR EXPR EXPR 
            | Codecopy     EXPR EXPR EXPR
            | Returndatacopy EXPR EXPR EXPR 
            | Extcodecopy
            deriving (Eq, Read) 
--} 








