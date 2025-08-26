module Action2Opcode where 

import Prelude hiding (EQ,LT,GT) 
import Hex 
import Node
import Action 
import Opcode 
import PreLink 
import Expr2Opcode
import GCLL
import Crypto (dispatcherHash) 


e2o = expr2opcode

dispatch s = [DUP1, PUSH4 (fromHex $ dispatcherHash s), EQ] 


revert = undefined 

-- #TODO 
enter           = [] -- make empty frame 
exit            = [] -- return stacktop; remove frame; push stacktop 
pushFUNSTACK i  = [PUSHFUNSTACK i ] 
popFUNSTACK     = [POPFUNSTACK ]





swap i = case i of 
    1   -> SWAP1
    2   -> SWAP2
    3   -> SWAP3
    4   -> SWAP4
    5   -> SWAP5
    6   -> SWAP6
    7   -> SWAP7
    8   -> SWAP8
    9   -> SWAP9
    10  -> SWAP10
    11  -> SWAP11
    12  -> SWAP12
    13  -> SWAP13
    14  -> SWAP14
    15  -> SWAP15
    16  -> SWAP16


bop o   = case o of  
    "=="  ->  [EQ] 
    "!="  ->  [EQ, NOT] 
    "<"   ->  [LT] 
    ">"   ->  [GT] 
    "<="  ->  [GT, NOT]
    ">="  ->  [LT, NOT] 
    "+"   ->  [ADD]
    "-"   ->  [SUB] 
    "*"   ->  [MUL]
    "/"   ->  [SUB]
    "%"   ->  [MOD] 

action2opcode :: Action -> [OPCODE] 
action2opcode a = case a of 
    AcStop                  -> [STOP]
    AcDispatch s            -> dispatch s
    AcRevert e1 e2          -> e2o e1 ++ e2o e2 ++ [REVERT] 
    AcReturn e1 e2          -> e2o e1 ++ e2o e2 ++ [RETURN] 
    AcPop                   -> [POP] 
    AcPush e                -> e2o e 
    AcSwap i                -> [swap i]  
    AcDup i                 -> [dup i]  
    AcBop s                 -> bop s 
    AcCalldatacopy x y z    -> e2o x ++ e2o y ++ e2o z ++ [CALLDATACOPY]  
    AcCodecopy x y z        -> e2o x ++ e2o y ++ e2o z ++ [CODECOPY] 
    AcExtcodecopy x y z w   -> e2o x ++ e2o y ++ e2o z ++ e2o w ++ [EXTCODECOPY] 
    AcSkip                  -> []   
    AcBool e                -> e2o e 
    AcElse                  -> [] 
    AcEnter                 -> enter 
    AcExit                  -> exit  
    AcVar i                 -> undefined 
    AcRecord (Q i)(Q j)     -> pushFUNSTACK i ++ pushFUNSTACK j   
    AcCheck  (Q i)(Q j)     -> popFUNSTACK ++ [PUSH16 (toInteger j), EQ] ++ popFUNSTACK ++ [PUSH16 (toInteger i), EQ]  
    AcSto i                 -> [PUSH0, PUSH (INT (toInteger i)), SSTORE] 
    -- a   -> [] 
    a                       -> error $ "action2opcode: [Undefined Arg] " ++ show a    


actions2opcodes = concat . map action2opcode 


{--
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
                | AcRecord (Node Int) (Node Int)
                | AcCheck  (Node Int) (Node Int)     
                | AcSeq [Action] 
                deriving (Eq, Read) 
                
instance Show Action where 
    show (AcStop                ) = "STOP"
    show (AcDispatch s          ) = s 
                
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
    show (AcRecord (Q i)(Q j)   ) = "RC" ++ show i ++ "/" ++ show j
    show (AcCheck  (Q i)(Q j)   ) = "CK" ++ show i ++ "/" ++ show j




--} 
