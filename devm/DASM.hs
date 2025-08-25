module Main where 


import Lex  (bytes) 
import Disasm
import Knit
import LTS
import Stmt


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


