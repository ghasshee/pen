

module ListOr where 

data ListOr a   = EMPTY
                | CONS a 
                | OR (ListOr a) (ListOr a) 


-- algebraic graph
