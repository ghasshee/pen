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
import Analysis 
        ( decomposedPaths, junctionNodes, bifurcationNodes, initialNodes, terminalNodes, confluenceNodes, success
        , genMat, convert, star', nodeReduction
        , innerizeOR, reNodeMat, Branch, branch )
import Action 
import Semiring 
import OR
import Action2Opcode
import Crypto 
import Asm 

import Print

import System.IO 
import System.Environment 
import Data.Char



-- Later implementation 
import Layout
import MSO
import Automata 
import ATree
import VC





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
    
    let ms      :: [Matrix (OR Action)] 
        ms      = map nodeReduction as

    let ns      :: [Matrix (Edge Int (OR Action))] 
        ns      = map reNodeMat ms

    let bs      :: [[Branch Action]] 
        bs      = map branch ns 

    print "------ Abstract Syntax Tree -------"
    print asts

    print "------- transpiled into Functional Term -------" 
    print tm 

    print "------ Program Graph -------" 
    print pgs 

    print "------ Variable Tree -------"
    print vts 
    
    print "------ Matrix Representation ----" 
    print $ convert <$> mats

    print "------ Diags ------"
    mapM printDiag mats

    print "------ Diags (Action ONLY) ------" 
    mapM printDiag' mats 

    print "------ Fixpoint' ------"
    print stars

    print "------ Matrix Edge Representation -----" 
    print mats 
    print as 

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

    print "------ ReNodeMat ------" 
    print $ ns 

    print "------ Branches -----"
    print $ bs

    --print $ map lu ms 
    --print $ map lu as 

    print "----- Crypto Test ----"
    print "set(uint256) hash is: " 
    print $ dispatcherHash "set(uint256)" 

    
