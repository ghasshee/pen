module Main where 


import Lex  (bytes) 
import Disasm
import Knit
import LTS


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
    let progs   = cut $ extract prog 
    mapM print $ map reverse progs 


    -- +-----------+ --
    -- | Decompile | --
    -- +-----------+ --
    let asts       = knits progs
    print asts


    -- +-----------------+ --
    -- | Numbering Nodes | --
    -- +-----------------+ -- 
    let numbered    = mkNode asts
    print numbered
