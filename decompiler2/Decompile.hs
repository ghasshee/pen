module Main where 


import Lex
import Tree
import Disasm
import Knit
-- import Eval


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

    let pr t = print t

    pr asts



