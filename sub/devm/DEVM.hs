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


    pr ""
    pr "-- +---------+ --"
    pr "-- | DisAsm  | --"
    pr "-- +---------+ --"
    pr ""
    let prog    = lineNo $ disAsm words 
    prAsm prog


    pr ""
    pr "-- +-----------------+ --"
    pr "-- | Cut into States | --"
    pr "-- +-----------------+ --"
    pr ""
    let progs   = revcut $ extract prog 
    mapM print $ map reverse progs 


    pr ""
    pr "-- +-----------+ --"
    pr "-- | Decompile | --"
    pr "-- +-----------+ --"
    pr ""
    let asts       = knits progs
    print asts

    pr ""
    pr "-- +-------------+ --"
    pr "-- |    GCLL     | --"
    pr "-- +-------------+ --"
    pr ""
    let stmts       = map optree2stmt asts 
    print stmts 

    pr ""
    pr "-- +-----------------+ --"
    pr "-- | GCLL with Nodes | --"
    pr "-- +-----------------+ --"
    pr ""
    let numbered    = mkNode stmts
    print numbered
