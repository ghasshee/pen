module Main where 


import Lex
import Disasm
import VM
import Link
import Let
import Tree


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
    let rmDUP       = fmap rmDUPs uncletrees 
    let rmSTACKTOP  = fmap rmSTACKTOPs rmDUP
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
    print $ fmap (fmap fst) rmSTACKTOP
    print "*****************************************************"
    print "***       Program Trees holding STACK Variables   ***"
    print "*****************************************************"
    print $ fmap (fmap fst) $ fmap rmSWAPs rmSTACKTOP



