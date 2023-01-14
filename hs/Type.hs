module Type where 


type ID     = String
data Ty     =   TyERR 
            |   TyUnit
            |   TyU256  -- 256 bits
            |   TyU8    --   8 bits
            |   TyI256  -- 256 bits
            |   TyI8    --   8 bits
            |   TyB32   -- 256 bits 
            |   TyBOOL
            |   TyPROD  [Ty] 
            |   TyABS   Ty Ty 
            |   TyMAP   Ty Ty 
            |   TyMTHD  ID [Ty] Ty
            |   TyDFLT
            |   TyREF   Ty
            |   TyVAR   ID Ty 

            |   TyADDR          -- 160 bits
            |   TyAMOUNT        -- type of Wei i.e. Ether
            |   TyINCR  Integer -- the balance of the account is increased 
            |   TyDECR  Integer -- the balance of the account is decreased 
            |   Untyped 
            deriving (Show, Eq, Read) 

