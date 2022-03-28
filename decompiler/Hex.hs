module Hex where 

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
toHex 0 = "00"
toHex n = toHex (div n 16) ++ [toHexChar (rem n 16)]

