module Hex where 

import Data.Char (isDigit)

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
fromhexChar 'A' = 10
fromhexChar 'B' = 11
fromhexChar 'C' = 12
fromhexChar 'D' = 13
fromhexChar 'E' = 14
fromhexChar 'F' = 15

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
toHexChar 10='A'  
toHexChar 11='B'  
toHexChar 12='C'  
toHexChar 13='D'  
toHexChar 14='E'  
toHexChar 15='F'  

toHex   :: Integer -> String 
toHex 0 = "0"
toHex n = loop n where 
    loop 0 = ""
    loop n = loop (div n 16) ++ [toHexChar (rem n 16)]


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
    _       -> isDigit c

