module Main where 

import Prelude hiding (lex, EQ, LT, GT) 

import Tree
import GCLL
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

    let mats :: [Matrix (OR Action)] 
        mats = map genMat pgs

    let bifurs = map (getBifurcationFrom 0) mats

    let ss  = map success mats 


    print "------ Abstract Syntax Tree -------"
    print ast
--    print "------- transpiled into Functional Term -------" 
--    print tm 
    print "------ Program Graph -------" 
    print pgs 
    

    print "------ Matrix Representation ----" 
    print mats


    print "------ Success Pathes ------"
    --mapM ( putStr . showListLn ) ss 
    print ss


    print "------ First Bifurcation -----" 
    print bifurs





    
