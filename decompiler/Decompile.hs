module Main where 


import Lex
import Tree
import Disasm
import Knit
import Let
-- import Link
--import RmVar
-- import Hoist
import Eval


main = do 
    -- | Lexer 
    words <- bytes 
    -- | DisAsm 
    let prog    = lineNo $ disAsm words 
    prAsm prog
    -- | Cut Asm 
    let progs   = cut $ extract prog 
    mapM print $ progs 
    -- | Decompile
    let asts       = knits progs

    -- | Link JUMPing Segments
    -- let linked      = link trees
    -- let vartrees    = mapvar linked
    -- let uncletrees  = fmap uncleVars vartrees 
    -- let linked'     = link trees' 
    -- print "*****************************************************"
    -- print "***       Concatenated JUMPing Segments           ***"
    -- print "*****************************************************"
    -- print linked
    -- let evaltree'   = eval $ fmap absPUSH trees'           

    -- | locally Variable Bind
    let vartrees        = fmap local asts 
    -- | Show Variable Stack States  
    --let uncletrees      = fmap uncleVars    vartrees 
    --let popRemoved      = fmap rmPOPs       uncletrees 
    --let dupRemoved      = fmap rmDUPs       popRemoved 
    --let swapRemoved     = fmap rmSWAPs      dupRemoved
    --let stackRemoved    = fmap rmSTACKs     swapRemoved
    --let varRemoved      = fmap (rmVARs . rmVARs . rmVARs )      stackRemoved
    --let trees           = fmap unUncle      varRemoved 

    let pu t = print $ fmap (fmap fst) t
    let pr t = print t

    pr asts
    pr "*****************************************************"
    pr "***       LET Segment Trees                       ***"
    pr "*****************************************************"
    pr vartrees
--    pr "*****************************************************"
--    pr "***       Decompiled Segment Trees                ***"
--    pr "*****************************************************"
--    pu uncletrees
--    pr "*****************************************************"
--    pr "***       POP removed !                           ***"
--    pr "*****************************************************"
--    pu popRemoved  
--    pr "*****************************************************"
--    pr "***       DUPs removed !                          ***"
--    pr "*****************************************************"
--    pu dupRemoved
--    pr "*****************************************************"
--    pr "***       SWAPs removed !                          ***"
--    pr "*****************************************************"
--    pu swapRemoved
--    pr "*****************************************************"
--    pr "***       STACK removed !                         ***"
--    pr "*****************************************************"
--    pu stackRemoved
--    pr "*****************************************************"
--    pr "***       VAR removed !                           ***"
--    pr "*****************************************************"
--    pr trees



