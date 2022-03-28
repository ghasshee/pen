module Main where 


import Lex
import Disasm
import VM
import Link
import Let
import Tree
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
    let trees       = knits progs
    -- | Link JUMPing Segments
    let linked      = link trees
    -- | Let Variable Binding 
    let vartrees    = mapvar linked
    -- | Show Variable Stack States  
    let uncletrees  = map uncleVars vartrees 
    let dupRemoved  = fmap rmDUPs uncletrees 
    let varRemoved  = fmap (rmVARs . rmSTACKTOPs) dupRemoved
    let swapRemoved = fmap (rmVARs . rmSWAPs) varRemoved
    let trees'      = fmap unUncle swapRemoved 
    -- let evaltree'   = eval $ fmap absPUSH trees'           
    let linked'     = link trees' 
    print "*****************************************************"
    print "***       LET Segment Trees                       ***"
    print "*****************************************************"
    print vartrees
    print "*****************************************************"
    print "***       Decompiled Segment Trees                ***"
    print "*****************************************************"
    print trees
    print "*****************************************************"
    print "***       Concatenated JUMPing Segments           ***"
    print "*****************************************************"
    print linked
    print "*****************************************************"
    print "***       Program Trees holding STACK Variables   ***"
    print "*****************************************************"
    print uncletrees
    print "*****************************************************"
    print "***       DUP Removed                             ***"
    print "*****************************************************"
    print $ fmap (fmap fst) varRemoved
    print "*****************************************************"
    print "***       Program Trees holding STACK Variables   ***"
    print "*****************************************************"
    print $ fmap (fmap fst) swapRemoved
    print "*****************************************************"
    print "***       Decompiled Segment Trees                ***"
    print "*****************************************************"
    print trees' 
    --print "*****************************************************"
    --print "***       Evaluated Trees                         ***"
    --print "*****************************************************"
    --print evaltree' 



