module Main where 

import Lex
import Asm


main :: IO () 
main = do 
    l <- asms

    putStrLn $ toBytes l 


