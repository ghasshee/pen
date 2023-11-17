module Config where 




data Index      =   Big  Integer     
                |   Int  Int 
                |   Label Int


data Datum a    =   D
                    { offset  :: a
                    , size    :: a 
                    }

data Loc    
