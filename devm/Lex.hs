module Lex where 

import System.IO 
import Data.Char

bytes :: IO [String]
bytes = loop [] where 
    loop l = do 
        e <- isEOF
        if e          then return $ reverse l else do 
            a <- getChar
            if a == '\n'  then return $ reverse l else do 
                if a == ' ' then loop (" ":l) else do 
                    b <- getChar 
                    loop $ [toUpper a,toUpper b] : l 

asms :: IO [String] 
asms = loop [""] where  
    loop (str:l) = do 
        e <- isEOF
        if e        then return $ reverse l else do 
            a <- getChar
            if a == '\n' then loop ([]:(reverse str:l)) else do 
                if a == ' ' then loop (" ":l) else do 
                    loop $ (a:str) : l 
