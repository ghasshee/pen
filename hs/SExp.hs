module SExp where 

data SExp   = SAtom String
            | SList [SExp]  
            deriving (Show, Eq, Read) 





