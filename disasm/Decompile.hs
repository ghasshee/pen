module Main where 


import Lex
import Disasm
import VM
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
    let tree    = map parse progs
    let ptree   = map elder_unclesRB tree
    print ptree
    
    print "*****************************************************"
    print "***       Generated Decompiled Tree               ***"
    print "*****************************************************"
    print tree


