module Hex where 

hexChar :: Char -> Integer
hexChar '0' = 0
hexChar '1' = 1
hexChar '2' = 2
hexChar '3' = 3
hexChar '4' = 4
hexChar '5' = 5
hexChar '6' = 6
hexChar '7' = 7
hexChar '8' = 8
hexChar '9' = 9
hexChar 'A' = 10
hexChar 'B' = 11
hexChar 'C' = 12
hexChar 'D' = 13
hexChar 'E' = 14
hexChar 'F' = 15


parseHex :: String -> Integer 
parseHex [] = 0 
parseHex str = hexChar (last str) + 16 * parseHex (init str)


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

toHex :: Integer -> String 
toHex 0 = ""
toHex n = toHex (div n 16) ++ [toHexChar (rem n 16)]

