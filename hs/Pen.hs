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
import Action 
import Semiring 
import OR
import Analysis 
        ( decomposedPaths, junctionNodes, bifurcationNodes, initialNodes, terminalNodes, confluenceNodes, success
        , genMat, convert, star', nodeReduction
        , innerizeOR, reNodeMat)
import Opt

import Branch (Branch, branch) 

import Opcode 
import Action2Opcode
import Branch2Opcode 

import Knit
import Optree2GCLL
import Codegen 

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
import CPS
import Kripke





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

    let decompss :: [[Matrix (OR Action)]] 
        decompss = map directSumDecompose ms 

    let bs      :: [[Branch Action]] 
        bs      = map branch ns 

    let ops     :: [[OPCODE]] 
        ops     = map branches2opcodes bs 

    let ops'    :: [[OPCODE]]
        ops'    = removeFUNSTACKOPCODEs $ ops

    let ops''   :: [[OPCODE]] 
        ops''   = undefined  



    let optrees :: [[RBTree OPCODE]]  
        optrees = map ( knits . revcut) ops'

    let gclls   :: [[GCLL]]
        gclls   = map optrees2stmts optrees 


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

    print "------ OPCODEs ------"
    print $ ops 

    print "------ remove FUNSTACK OPCODE -----"
    print $ ops' 

    print "------ OpTrees ------"
    print $ optrees 

    print "------ GCLLs  -------"
    print $ gclls 

    --print $ map lu ms 
    --print $ map lu as 
    
    print "----- Direct Sum Decompostions -----" 
    print $ map allComponents ms 
    print $ decompss

    print "----- Crypto Test ----"
    print "set(uint256) hash is: " 
    print $ dispatcherHash "set(uint256)" 

    
