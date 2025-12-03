module Utils where 

import Data.List (reverse, length)  

import Prelude hiding (reverse, length) 

hd :: [a] -> a  
hd []       = error "hd: empty list" 
hd (a:_)    = a 

tl :: [a] -> [a] 
tl []       = error "tl: empty list"
tl (_:as)   = as

rev a = reverse a 
len a = length a


fst3 (x,_,_) = x
snd3 (_,y,_) = y
thd3 (_,_,z) = z

double f x = f ( f x )

err = error 



to :: Int -> Integer 
to      = toInteger 

from :: Integer -> Int  
from = fromInteger 


infixr 6 &&$ 
infixr 6 ||$

(&&$) f g v = (&&) (f v) (g v) 
(||$) f g v = (||) (f v) (g v)

infixr 1 ¬
(¬) a   = not a 
