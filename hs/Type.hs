module Type where 


import Numeric.Natural 
import Prelude hiding ((<$))

type N = Natural 

type ID     = String
data Ty     =   TyERR   --   nothing 
            |   TyUNIT  --   0 bit 
            |   TyBOOL  --   1 bit
            |   TyNAT
            |   TyU8    --   8 bits
            |   TyI8    --   8 bits
            |   TyADDR  -- 160 bits
            |   TyU256  -- 256 bits
            |   TyI256  -- 256 bits
            |   TyB32   -- 256 bits 
            |   TyPROD  [Ty] 
            |   TyABS   ID Ty    -- λX.T
            |   TyREC   ID Ty    -- μX.T  
            |   TyARR   Ty Ty    -- T → T  
            |   TyMTHD  ID [Ty] Ty
            |   TyDATA  ID [Ty] -- User Defined Datatype -- Datatype.hs
            |   TyAPP Ty Ty 
            -- |   TyDFLT
            |   TyREF   Ty     
            |   TySRC   Ty      |   TySINK Ty 
            |   TyID    ID 
            |   TyVAR   Int 
            |   TyTOP           -- forall T. T 
            |   TyAMOUNT        -- type of Wei i.e. Ether
            |   TyINCR  Integer -- the balance of the account is increased 
            |   TyDECR  Integer -- the balance of the account is decreased 
            |   TySEND 
            |   TyTRANSFER
            |   Ty String  
            |   TyPoly String [Ty] 
            |   Untyped 
            deriving (Eq, Read) 


instance Show Ty where 
    show ty = case ty of 
        TyERR       -> "𝟎"
        TyUNIT      -> "𝟏" 
        TyBOOL      -> "𝟐"
        TyNAT       -> "𝐍"
        TyARR a b   -> show a ++ " → " ++ show b
        TyID id     -> id 
        TyAPP a b   -> "TyAPP " ++ show a ++ " " ++ show b 
        TyVAR i     -> show i 
        TyABS x ty  -> "λ" ++ show x ++ "." ++ show ty
        TyREC x ty  -> "μ" ++ show x ++ "." ++ show ty 
        TyU8        -> "u8"
        TyU256      -> "uint" 
        Untyped     -> "Untyped" 
        e           -> error $ show e 




showTy ty = case ty of 
    TyBOOL      -> "bool"
    TyU8        -> "uint8"
    TyADDR      -> "address"
    TyU256      -> "uint256"
    TyI256      -> "int256" 


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


