module Pattern where



data Pattern        =   PVar String             -- x 
                    |   PWildcard               -- _ 
                    |   PCon String [Pattern]   -- C p1 .. pk
                    deriving (Eq, Show, Read ) 
