module Variable where







data Variable   = X Int -- NonRec    variable
                | F Int -- Function  variable
                | R Int -- Recursive variable
                | H Int -- Hidden    Variable
                | S Int -- Storage   Variable
                | M Int -- Memory    Variable 
                | A Int -- Method    Variable
                | C Int -- Contract  Variable
                deriving (Show) 



