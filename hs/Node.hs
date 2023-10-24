module Node where 


-- || Ddfinition || 

data Node s = Q s                 
            deriving (Eq,Ord,Read) 

-- instance {-# Overlapping #-} Ord s => Eq (Node [s]) where 
--     Q xs     == Q ys     = sort xs == sort ys
    

instance Num s => Num (Node s) where 
    fromInteger i = Q (fromInteger i) 
    Q i + Q j     = Q (i + j) 
    Q i * Q j     = Q (i*j) 
    negate (Q i)  = Q (negate i) 
    signum (Q i)  = Q (signum i) 
    abs (Q i )    = Q (abs i) 

instance Show s => Show (Node s) where 
    show (Q s) = "Q" ++ show s 

type Edge s a = (Node s,a,Node s) 

