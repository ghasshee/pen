module Data where 





import Hex
import Type 
import Utils 



-- Inductive Data Type 
data DInd       = DInd ID [ID] [DConstr] 
                deriving (Show, Eq, Read) 

data DConstr    = DConstr ID [Ty] 
                deriving (Show, Eq, Read) 



-- e.g. 
-- defining List 

l = DInd "List" ["a"] 
        [DConstr "Nil"  []
        ,DConstr "Cons" [TyID "a", TyAPP (TyID "List") (TyID "a")]
        ] 

n = DInd "Nat" []
        [DConstr "Succ" [TyID "Nat"]
        ,DConstr "Zero" []
        ]




-- Old experimental Data 
-- #TODO remove this Data definition 
data Data   = DSucc Data
            | DZero 
            deriving (Eq, Read) 

instance Show Data where 
    show (DSucc d)  = "S" ++ show d
    show (DZero  )  = "O"

data2nat :: Data -> Integer
data2nat (DZero)   = 0 
data2nat (DSucc d) = 1 + data2nat d

-- #END_TODO 




    {--

# solc 

# mapping 
# variable 

--}

-- data Patricia   = Undefined 

{--
type mapping = address -> uint 
--}






{--
contract bank { 

method withdraw (amount : Wei) {

    data Tree  = Leaf amount 
               | Node account Tree Tree 


--}



