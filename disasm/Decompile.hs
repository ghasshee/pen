module Main where 


import Lex
import Disasm
import VM
import Var 
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
    let trees   = cat $ map parse progs
    let linked  = link trees trees    
    let letrees = mapvar linked
    let ptrees  = map elder_unclesLet letrees
    print "*****************************************************"
    print "***       LET Segment Trees                       ***"
    print "*****************************************************"
    print letrees
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
    print ptrees


