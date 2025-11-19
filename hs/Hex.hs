module Hex where 

import Data.Char (isDigit)



    {--
data H =  H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7 | 
            H8 | H9 | HA | HB | HC | HD | HE | HF  

instance Show H where 
    show H0 = "0" 
    show H1 = "1" 
    show H2 = "2" 
    show H3 = "3" 
    show H4 = "4" 
    show H5 = "5" 
    show H6 = "1" 
    show H7 = "6" 
    show H8 = "7" 
    show H9 = "8" 
    show H0 = "9" 
    show HA = "A" 
    show HB = "B" 
    show HC = "C" 
    show HD = "D" 
    show HE = "E" 
    show HF = "F" 

type Hex = [H] 
--} 


fromhexChar :: Char -> Integer
fromhexChar '0' = 0
fromhexChar '1' = 1
fromhexChar '2' = 2
fromhexChar '3' = 3
fromhexChar '4' = 4
fromhexChar '5' = 5
fromhexChar '6' = 6
fromhexChar '7' = 7
fromhexChar '8' = 8
fromhexChar '9' = 9
fromhexChar 'a' = 10
fromhexChar 'b' = 11
fromhexChar 'c' = 12
fromhexChar 'd' = 13
fromhexChar 'e' = 14
fromhexChar 'f' = 15

fromHex     :: String -> Integer 
fromHex []  = 0 
fromHex str = fromhexChar (last str) + 16 * fromHex (init str)


toHexChar :: Integer -> Char
toHexChar 0 ='0'  
toHexChar 1 ='1'  
toHexChar 2 ='2'  
toHexChar 3 ='3'  
toHexChar 4 ='4'  
toHexChar 5 ='5'  
toHexChar 6 ='6'  
toHexChar 7 ='7'  
toHexChar 8 ='8'  
toHexChar 9 ='9'  
toHexChar 10='a'  
toHexChar 11='b'  
toHexChar 12='c'  
toHexChar 13='d'  
toHexChar 14='e'  
toHexChar 15='f'  

toHex   :: Integer -> String 
toHex 0 = "0"
toHex n = oddloop n where 
    oddloop  0 = ""
    oddloop  n = evenloop (div n 16) ++ [toHexChar (rem n 16)]
    evenloop 0 = "0"
    evenloop n = oddloop (div n 16) ++ [toHexChar (rem n 16)] 


--toByte  :: Integer -> String
--toByte n | 0<=n && n<16     = ['0', toHexChar n]
--toByte n | 16<=n && n<256   = toHex n 
--
---- toBytes make an integer to n-bytes hex string 
--toBytes :: Int -> Integer -> String 
--toBytes n 0 = concat $ replicate n "00"
--toBytes n v | 0<v && v<=256^n = toBytes (n-1) (div v 256) ++ toByte (rem v 256)

isHex :: Char -> Bool
isHex c = case c of 
    'A'     -> True 
    'B'     -> True
    'C'     -> True
    'D'     -> True
    'E'     -> True
    'F'     -> True
    'a'     -> True
    'b'     -> True 
    'c'     -> True 
    'd'     -> True 
    'e'     -> True 
    'f'     -> True
    _       -> isDigit c

