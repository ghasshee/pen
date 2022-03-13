module Main where 


import Lex
import Disasm
import VM


main = do 
    words <- bytes 
    let prog = lineNo $ disAsm words 
    prAsm prog
    let progs = cuts $ extract prog 
    mapM print $ progs 
    let tree = fparse progs
    print tree


