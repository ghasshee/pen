module Pattern where



data Pattern        =   PWild                   -- _ 
                    |   PVar String             -- x 
                    |   PCon String [Pattern]   -- C p1 .. pk
                    deriving (Eq, Show, Read ) 
