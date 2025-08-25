module Main where 


import Lex  (bytes) 
import Opcode
import Tree
import Disasm
import Knit
import Node
import Stmt 


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
    let prog    :: [(Integer, OPCODE)] 
        prog    = lineNo $ disAsm words 
    prAsm prog


    pr ""
    pr "-- +-----------------+ --"
    pr "-- | Cut into States | --"
    pr "-- +-----------------+ --"
    pr ""
    let progs   :: [[OPCODE]] 
        progs   = revcut $ extract prog 
    mapM print $ map reverse progs 


    pr ""
    pr "-- +-----------+ --"
    pr "-- | Decompile | --"
    pr "-- +-----------+ --"
    pr ""
    let asts    :: [RBTree OPCODE]
        asts    = knits progs
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
    let numbered    = zipNode stmts
    print numbered
