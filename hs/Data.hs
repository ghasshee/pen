module Data where 




data Data   = DSucc Data
            | DZero 
            deriving (Eq, Read) 


instance Show Data where 
    show (DSucc d)  = "S" ++ show d
    show (DZero  )  = "O"




data2nat :: Data -> Integer
data2nat (DZero)   = 0 
data2nat (DSucc d) = 1 + data2nat d


