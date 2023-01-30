module Main where 

import Prelude hiding (lex, EQ, LT, GT) 

import Tree
import GCLL
import Type
import Term
import Lexer
import Parser
import Typing
import PG
import Decl2Term

import System.IO 
import System.Environment 
import Data.Char


main = do 
    args <- getArgs 
    let (file:_) = args
    contents <- readFile file 

    let ast = parse . lex $ contents
    let tm  = map transpileCN ast 


    print ast
    print "--------------" 
    print tm 


    
