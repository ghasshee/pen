module PreLink where 



data PreLinkValue   = LABEL Int             -- JUMPDEST Label 
                    | FUN Int               -- Function Stack Number
                    | CR_SIZE               -- creation codesize
                    | RN_SIZE               -- runtime codesize 
                    | LIB_ADDR              -- library address
                    | INT Integer 
                    deriving (Show, Eq, Read) 




