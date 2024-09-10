module DCont where 


data DCont a    = DContL (RBTree a) (DCont a) 
                | DContR Int        (DCont a) 
                | DContID 





