module Main where 

import Prelude hiding (lex, EQ, LT, GT) 

import Tree
import GCLL
import Type
import Term
import Lexer
import Parser
import TypingRule
import PG

import System.IO 
import System.Environment 
import Data.Char


main = do 
    args <- getArgs 
    let (file:_) = args
    contents <- readFile file 

    let ast = parse . lex $ contents
    print ast


    
