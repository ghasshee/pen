{-# LANGUAGE FlexibleContexts #-} 
module Main where 

import Prelude hiding (lex, EQ, LT, GT) 

import Tree
import GCLL hiding (M)
import Type
import AST
import Term
import Lexer
import Parser
import Typing
import PG
import Decl2Term
import Mat
import MatRep
import Action 
import Semiring 
import OR

import System.IO 
import System.Environment 
import Data.Char

printDiag a@(M n _ _ _ _ _) = loop a a n where 
    loop a an n = do 
        print $ show n ++ ": " 
        print $ diag an 
        let a'  = rmLoops an a 
        let an' = mult a' an 
        if n /= 0 then loop a' an' (n-1) else return ()  

printStar a@(M n _ _ _ _ _) = loop a a n where 
    loop a an n = do 
        print $ show n ++ ":\n" 
        print $ convert an 
        print "success path: " 
        print $ success an  
        let a'  = rmLoops an a 
        let an' = mult a an 
        let s   = success an' 
        if n /= 1 then loop a' an' (n-1) else return () 

main = do 
    args <- getArgs 
    let (file:_) = args
    contents <- readFile file 

    let ast :: [CONTRACT] 
        ast     = parse . lex $ contents

    let tm  :: [Term] 
        tm      = map transpileCN ast 

    let pgs :: [Edges] 
        pgs     = map mkPG ast

    let mats :: [Matrix (OR Transition)] 
        mats    = map genMat pgs

    let stars :: [Matrix (OR Transition)] 
        stars   = map star' mats


    let ss      = map success stars
        

    print "------ Abstract Syntax Tree -------"
    print ast
--    print "------- transpiled into Functional Term -------" 
--    print tm 
    print "------ Program Graph -------" 
    print pgs 
    
    print "------ Matrix Representation ----" 
    print $ map convert mats

    print "------- Stars -------"
    mapM printStar mats  

    print "------ Diags ------"
    mapM printDiag mats

    print "------ Fixpoint' ------"
    print stars




    
