module Main where 


import Lex
import Disasm
import VM


main = do 
    -- | Lexer 
    words <- bytes 
    -- | DisAsm 
    let prog = lineNo $ disAsm words 
    prAsm prog
    -- | Cut Asm 
    let progs = cuts $ extract prog 
    mapM print $ progs 
    -- | Decompile
    let tree = fparse progs
    print tree


