{-# LANGUAGE FlexibleContexts #-} 
module Main where 

import Prelude hiding (lex, EQ, LT, GT) 

import Lexer
import Parser

import Tree
import Type
import Term
import AST

import Bind 
import Typing
import Eval 

import Edge
import GCLL hiding (M)
import PG

import Decl2Term
import VarTree

import Mat
import Action 
import Semiring 
import OR
import Analysis 
        ( decomposedPaths, junctionNodes, bifurcationNodes, initialNodes, terminalNodes, confluenceNodes, success
        , genMat, convert, star', nodeReduction
        , innerizeOR, reNodeMat)
import PG2DOT 
import Branch (Branch, branch) 

import Opcode 
import Action2Opcode
import Branch2Opcode 


import Disasm hiding (pr) 
import Knit
import Optree2GCLL
import Codegen 

import Crypto 
import Asm 


import ABI 



import Print
import Utils 

import System.IO 
import System.Environment 
import Data.Char



-- Later implementation 
import Layout
import MSO
import Automata hiding (trim) 
import ATree
import VC
import Kripke


type Args = [String] 

subcmds []              = error "file not specifies" 
subcmds (a:as)          = do
    case a of 
        "--abi"         ->  do 
                            contents    <- subcmds as 
                            putStrLn $ abis contents 
                            return [] 
        "--bin"         ->  do 
                            contents    <- subcmds as 
                            let [bin] = bytes contents 
                            putStrLn $ bin  
                            return [] 
        "--graph"       ->  do 
                            contents    <- subcmds as 
                            let (g:_)     = dots contents 
                            putStrLn $ g
                            let (m:_) = ms $ contents   
                            writeDot m 
                            return [] 
        file            ->  do 
                            contents <- readFile file 
                            return contents 
      
       
                            



main = do 
    args <- getArgs 
    -- let (file:_) = args
    -- contents <- readFile file 
    -- pr contents 
    contents <- subcmds args 
    case contents of 
      []    -> return () 
      _     -> pr contents 

asts   :: String -> [CONTRACT]
asts   = parse . lex 

abis    :: String -> String 
abis    = abi . asts 

typed   :: String -> [CONTRACT] 
typed   = map processCN . asts 

tm      :: String -> [Term] 
tm      = map transpileCN . typed 

pgs     :: String -> [PG] 
pgs     = map pg . typed 

vts     :: String -> [VT] 
vts     = map mkVT . typed 

mats    :: String -> [Matrix (OR (Edge Int Action))] 
mats    =  map genMat . pgs  
    
as      = map convert . mats 
stars   = map star' . mats 
ss      = map success . stars 
ms      = map nodeReduction . as 
ns      = map reNodeMat . ms 
dots    = map renderPG . ms 
decomps     = map directSumDecompose . ms 
decomps'    = map directSumDecompose . ns  
ns'     = hd . decomps' 
bs      = map branch . ns' 
ops     = map branches2opcodes . bs 
ops'    = rmFUNSTACKs . ops 
ops''   = map codegen . ops' 
bytes   = map asm . ops'' 
dasms   = map extract . map ( lineNo . disAsm . byte ) . bytes 
optrees = map (knits . revcut) . dasms
gclls   :: String -> [[GCLL]] 
gclls   = map optrees2stmts . optrees 

pr contents = do 
    -- AST.hs 
    let asts    :: [CONTRACT] 
        asts    = parse . lex $ contents
    -- ABI.hs 
    let abis    :: String 
        abis    = abi asts 
    -- Eval.hs
    let typed   = map processCN asts 
    -- Term.hs 
    let tm      :: [Term] 
        tm      = map transpileCN typed
    -- PG.hs 
    let pgs     :: [PG] 
        pgs     = map pg typed
    -- VarTree.hss
    let vts     :: [VT] 
        vts     = map mkVT typed 

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

    let dots    = map renderPG ms  

    let ns      :: [Matrix (Edge Int (OR Action))] 
        ns      = map reNodeMat ms

    let decompss :: [[Matrix (OR Action)]] 
        decompss = map directSumDecompose ms 

    let ns'     :: [Matrix (Edge Int (OR Action))] 
        ns'     =  hd (directSumDecompose <$> ns)

    let bs      :: [[Branch Action]] 
        bs      = map branch ns'

    let ops     :: [[OPCODE]] 
        ops     = map branches2opcodes bs 

    let ops'    :: [[OPCODE]]
        ops'    = rmFUNSTACKs $ ops

    let ops''   :: [[OPCODE]]
        ops''   = codegen <$> ops'

    let bytes   :: [String] 
        bytes   = asm <$> ops'' 

    let dasms   :: [[OPCODE]]
        dasms   = extract <$> ((lineNo . disAsm . byte) <$>  bytes) 

    let optrees :: [[RBTree OPCODE]]  
        optrees = map ( knits . revcut) dasms

    let gclls   :: [[GCLL]]
        gclls   = map optrees2stmts optrees 

    print "------ Abstract Syntax Tree -------"
    print $ asts 
    print "------ Typed   AST   -----"
    print $ typed 

    -- print "------- transpiled into Functional Term -------" 
    -- print tm 

    print "------ Program Graph -------" 
    print $ pgs  
        {--
    print "------ Variable Tree -------"
    print $ vts  
   
    print "------ Matrix Representation ----" 
    print $ map convert $ mats 
   
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
--} 
    print "------ Node Reduction ------" 
    print $ ms 

    print "----- Direct Sum Decompostions -----" 
    print "trims:"
    print $ map (\m -> trim m 1 2)  ms 
    print "decomposions:"
    print $ map allComponents ms  
    print $ decompss

{--
    print "------ ReNodeMat ------" 
    print $ ns 
--}
    print "------ Branches -----"
    print $ bs  
   
    print "------ OPCODEs ------"
    print $ ops  
   
    print "------ remove FUNSTACK OPCODE -----"
    print $ ops'  
   
    print "------ with Address -----"
    print $ ops''  
        {--
    print "------ OpTrees ------"
    print $ optrees  
--}
    print "------ GCLLs  -------"
    print $ gclls 
         
    print "------ ABI -------" 
    putStrLn $ abis  
           
    print "------ EVM ByteCodes ------"
    print $ bytes 

    print "----- Crypto Test ----"
    print "set(uint256) hash is: " 
    print $ dispatcherHash "set(uint256)" 




    
    putStrLn ""
    putStrLn "======================================================="
    putStrLn ""


    
