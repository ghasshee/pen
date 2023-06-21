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

import System.IO 
import System.Environment 
import Data.Char


main = do 
    args <- getArgs 
    let (file:_) = args
    contents <- readFile file 

    let ast :: [CONTRACT] 
        ast = parse . lex $ contents
    let tm  = map transpileCN ast 

    let pgs = map mkPG ast

    let mat = map genMat pgs

    let ss  = map success mat 


    print "------ Abstract Syntax Tree -------"
    print ast
--    print "------- transpiled into Functional Term -------" 
--    print tm 
    print "------ Program Graph -------" 
    print pgs 
    

    print "------ Matrix Representation ----" 
    print mat


    print "------ Success Pathes ------"
    --mapM ( putStr . showListLn ) ss 
    print ss





    
