module Config where 







data Datum a    =   D
                    { offset  :: a
                    , size    :: a 
                    }




data Location   =   Stor  (Datum Int) 
                |   Mem   (Datum Int)
                |   Stack (Datum Int) 



type Config     =   [(Location, Integer)] 



                    



