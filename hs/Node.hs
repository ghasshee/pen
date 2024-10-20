

module Node where 


import Set 
import Mapping

-- || Ddfinition || 

data Node s = Q s                 
            deriving (Eq,Ord,Read) 

unNode :: Node s -> s 
unNode(Q s) = s 
-- instance {-# Overlapping #-} Ord s => Eq (Node [s]) where 
--     Q xs     == Q ys     = sort xs == sort ys
    

instance Enum s => Enum (Node s) where 
    succ (Q s)      = Q (succ s) 
    toEnum s        = Q (toEnum s) 
    fromEnum (Q s)  = fromEnum s

instance Num s => Num (Node s) where 
    fromInteger i = Q (fromInteger i) 
    Q i + Q j     = Q (i + j) 
    Q i * Q j     = Q (i*j) 
    negate (Q i)  = Q (negate i) 
    signum (Q i)  = Q (signum i) 
    abs (Q i )    = Q (abs i) 

instance Show s => Show (Node s) where 
    show (Q s)      = "Q" ++ show s 


renode_table :: (Num s, Enum s) => [node] -> Map node (Node s)
renode_table nodes = table nodes (Q <$> [0..]) 


-- renode function 
renode :: Eq node => Map node (Node s) -> node -> Node s 
renode = mapping 

renodes :: Eq node => Map node (Node s) -> [node] -> [Node s] 
renodes = mapping_all  

    

