module Main where 


import Lex  (bytes) 
import Disasm
import Knit
import LTS
import Term


main = do 

    -- +-------+ --
    -- | Lexer | --
    -- +-------+ --
    words <- bytes 


    -- +---------+ --
    -- | DisAsm  | --
    -- +---------+ --
    let prog    = lineNo $ disAsm words 
    prAsm prog


    -- +-----------------+ --
    -- | Cut into States | --
    -- +-----------------+ --
    let progs   = revcut $ extract prog 
    mapM print $ map reverse progs 


    -- +-----------+ --
    -- | Decompile | --
    -- +-----------+ --
    let asts       = knits progs
    print asts

    let stmts       = map optree2stmt asts 
    print stmts 

    -- +-----------------+ --
    -- | Numbering Nodes | --
    -- +-----------------+ --

    let numbered    = mkNode stmts
    print numbered
