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
    let tree    = map parse progs
    let lettree = map var tree
    let ptree   = map elder_unclesLet lettree
    print lettree
    print "*****************************************************"
    print "***       Generated LET TREE                      ***"
    print "*****************************************************"
    print ptree
    print "*****************************************************"
    print "***       Generated Decompiled Tree               ***"
    print "*****************************************************"
    print tree


