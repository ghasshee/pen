module Tbl where 


import qualified Data.HashTable.IO as H


type HashTbl k v = H.BasicHashTable k v

type Tbl = HashTbl Integer String

init :: IO Tbl
init =  H.new 

insert :: Integer -> String -> IO Tbl -> IO Tbl
insert k v tbl = do 
    t <- tbl 
    H.insert t k v 
    return t

lookup :: Integer -> IO Tbl -> IO (Maybe String)
lookup k mem   = do 
    tbl <- mem 
    v <- H.lookup tbl k 
    return v
    


