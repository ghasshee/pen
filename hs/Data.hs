module Data where 




data Data   = DSucc Data
            | DZero 
            deriving (Eq, Read) 


instance Show Data where 
    show (DSucc d)  = "S" ++ show d
    show (DZero  )  = "O"
