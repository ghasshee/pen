module Type where 


type ID     = String
data Ty     =   TyERR   --   nothing 
            |   TyUNIT  --   0 bit 
            |   TyBOOL  --   1 bit
            |   TyU8    --   8 bits
            |   TyI8    --   8 bits
            |   TyADDR  -- 160 bits
            |   TyU256  -- 256 bits
            |   TyI256  -- 256 bits
            |   TyB32   -- 256 bits 
            |   TyPROD  [Ty] 
            |   TyABS   Ty Ty 
            |   TyMAP   Ty Ty 
            |   TyMTHD  ID [Ty] Ty
            |   TyDFLT
            |   TyREF   Ty
            |   TyVAR   ID Ty 

            |   TyAMOUNT        -- type of Wei i.e. Ether
            |   TyINCR  Integer -- the balance of the account is increased 
            |   TyDECR  Integer -- the balance of the account is decreased 
            |   Ty String  
            |   TyPoly String [Ty] 
            |   Untyped 
            deriving (Show, Eq, Read) 



ty2num TyERR    = 0 
ty2num TyUNIT   = 1 
ty2num TyBOOL   = 2 
ty2num TyU8     = 3 
ty2num TyI8     = 4
ty2num TyADDR   = 5
ty2num TyU256   = 6
ty2num TyI256   = 7 
ty2num TyB32    = 8 
ty2num _        = error "not an atomic type" 

