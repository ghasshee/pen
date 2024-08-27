{-# LANGUAGE FlexibleContexts #-} 
module Main where 

import Prelude hiding (lex, EQ, LT, GT) 

import Tree
import Edge
import GCLL hiding (M)
import Type
import AST
import Term
import Lexer
import Parser
import Typing
import PG
import VarTree
import Decl2Term
import Mat
import MatRep
import Action 
import Semiring 
import OR

import Print

import System.IO 
import System.Environment 
import Data.Char




main = do 
    args <- getArgs 
    let (file:_) = args
    contents <- readFile file 

    -- AST.hs 
    let ast :: [CONTRACT] 
        ast     = parse . lex $ contents

    -- Term.hs 
    let tm  :: [Term] 
        tm      = map transpileCN ast 

    -- PG.hs 
    let pgs :: [Edges] 
        pgs     = map mkPG ast

    -- VarTree.hs
    let vts :: [VT] 
        vts     = map mkVT ast

    -- MatRep.hs
    let mats :: [Matrix (OR (Edge Int Action))] 
        mats    = map genMat pgs

    let stars :: [Matrix (OR (Edge Int Action))] 
        stars   = map star' mats

    let ss      = map success stars
        
    let loopEntrs = map loopEntranceNodes mats 

    print "------ Abstract Syntax Tree -------"
    print ast
--    print "------- transpiled into Functional Term -------" 
--    print tm 
    print "------ Program Graph -------" 
    print pgs 

    print "------ Variable Tree -------"
    print vts 
    
    print "------ Matrix Representation ----" 
    print $ convert <$> mats

    print "------- Stars -------"
    mapM printStar mats  

    print "------ Diags ------"
    mapM printDiag mats

    print "------ Diags (Action ONLY) ------" 
    mapM printDiag' mats 

    print "------ Fixpoint' ------"
    print stars

    print "------ Matrix Edge Representation -----" 
    print mats 

    print "------ Loop Entrances ------" 
    print loopEntrs




    
