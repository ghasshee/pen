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
import Matrix 
import Mat
import Action 
import Semiring 

import System.IO 
import System.Environment 
import Data.Char

printDiag a an n = do 
    print $ show n ++ ": " 
    print $ diag an 
    let an' = mult a an 
    if n /= 0 then printDiag a an' (n-1) else return ()  

printStars a an n = do 
    print $ show n ++ ":\n" 
    print $ an 
    print "success path: " 
    print $ success an  
    let a' = removeLoops an a 
    let an' = mult a an 
    let s  = success an' 
    if n /= 1 then printStars a' an' (n-1) else return () 

main = do 
    args <- getArgs 
    let (file:_) = args
    contents <- readFile file 

    let ast :: [CONTRACT] 
        ast = parse . lex $ contents

    let tm  :: [Term] 
        tm  = map transpileCN ast 

    let pgs :: [Edges] 
        pgs = map mkPG ast

    let mats :: [Matrix (OR Transition)] 
        mats = map genMat pgs

    let stars :: [Matrix (OR Transition)] 
        stars = map star' mats


--    let bifurs  = map (getBifurcationFrom 0) mats

--    let ss      = map success stars'  
        

    print "------ Abstract Syntax Tree -------"
    print ast
--    print "------- transpiled into Functional Term -------" 
--    print tm 
    print "------ Program Graph -------" 
    print pgs 
    
    print "------ Matrix Representation ----" 
    print mats

    print "------- Stars -------"
    mapM (\a@(M n _ _ _ _ _) -> printStars a a n) mats  

    print "------ Diags ------"
    mapM (\a@(M n _ _ _ _ _) -> printDiag a a n) mats

    print "------ Fixpoint' ------"
    print stars


    --print "------ Fixpoint -------"
    --print stars 

  --  print "------ First Bifurcation -----" 
 --   print bifurs





    
