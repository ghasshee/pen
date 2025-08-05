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
import Analysis (decomposedPaths, junctionNodes, bifurcationNodes, initialNodes, terminalNodes, confluenceNodes, success, genMat, convert, star', nodeReduction, innerizeOR, reNodeMat, Branch, branching)
import Action 
import Semiring 
import OR
import Action2Opcode
import Crypto 

import Print

import System.IO 
import System.Environment 
import Data.Char




main = do 
    args <- getArgs 
    let (file:_) = args
    contents <- readFile file 

    -- AST.hs 
    let asts    :: [CONTRACT] 
        asts    = parse . lex $ contents

    -- Term.hs 
    let tm      :: [Term] 
        tm      = map transpileCN asts

    -- PG.hs 
    let pgs     :: [Edges] 
        pgs     = map mkPG asts

    -- VarTree.hss
    let vts     :: [VT] 
        vts     = map mkVT asts

    -- Analysis.hs
    let mats    :: [Matrix (OR (Edge Int Action))] 
        mats    = map genMat pgs

    let as      :: [Matrix (OR Action)] 
        as      = map convert mats 

    let stars   :: [Matrix (OR (Edge Int Action))] 
        stars   = map star' mats

    let ss      = map success stars

    -- Action2Opcode.hs 
        
--    let loopEns = map loopEntranceNodes as 

    
    let ms      :: [Matrix (OR Action)] 
        ms      = map nodeReduction as

    let ms'     :: [Matrix (OR (Edge Int Action))] 
        ms'     = map nodeReduction mats

    let ms''    :: [Matrix (Edge Int (OR Action))] 
        ms''    = map (fmap innerizeOR) ms' 

    let ms'''   :: [Matrix (Edge Int (OR Action))] 
        ms'''   = map reNodeMat ms

    let bs      :: [[Branch Action]] 
        bs      = map branching ms''

    print "------ Abstract Syntax Tree -------"
    print asts
--    print "------- transpiled into Functional Term -------" 
--    print tm 
    print "------ Program Graph -------" 
    print pgs 

    print "------ Variable Tree -------"
    print vts 
    
    print "------ Matrix Representation ----" 
    print $ convert <$> mats

    --print "------- Stars -------"
    --mapM printStar mats  

    print "------ Diags ------"
    mapM printDiag mats

    print "------ Diags (Action ONLY) ------" 
    mapM printDiag' mats 

    print "------ Fixpoint' ------"
    print stars

    print "------ Matrix Edge Representation -----" 
    print mats 
    print as 

    --print "------ Loop Entrances ------" 
    --print loopEns

    print "------ Confluences ------" 
    print $ map confluenceNodes as 

    print "------ Bifurcations ------"
    print $ map bifurcationNodes as 

    print "------ junction Nodes ------"
    print $ map junctionNodes as 

    print "------ initial Nodes ------"
    print $ map initialNodes as 

    print "------ terminal Nodes -----" 
    print $ map terminalNodes as 
    
    print "------ decomposed path ------"
    print $ map decomposedPaths as

    print "------ Node Reduction ------" 
    print $ ms

    print "------ Node Reduction with Node Info ----" 
    print $ ms' 

    print "------ Edge Information rewrapping ------"
    print $ ms''

    print "------ ReNodeMat ------" 
    print $ ms'''

    print "------ Branches -----"
    print $ bs

    --print $ map lu ms 
    --print $ map lu as 

    print "----- Crypto Test ----"
    print "set(uint256) hash is: " 
    print $ dispatcherHash "set(uint256)" 

    
