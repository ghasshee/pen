module VM where

import Asm
import Tree



data Tm     = TmADD             Tm Tm
            | TmSUB             Tm Tm 
            | TmMUL             Tm Tm
            | TmDIV             Tm Tm 
            | TmSDIV            Tm Tm 
            | TmMOD             Tm Tm 
            | TmSMOD            Tm Tm
            | TmADDMOD          Tm Tm Tm
            | TmMULMOD          Tm Tm Tm
            | TmEXP             Tm Tm 
            | TmSIGNEXTEND      Tm Tm
            | TmLT              Tm Tm
            | TmGT              Tm Tm 
            | TmSLT             Tm Tm   
            | TmSGT             Tm Tm
            | TmEQ              Tm Tm   
            | TmISZERO          Tm 
            | TmAND             Tm Tm
            | TmOR              Tm Tm
            | TmXOR             Tm Tm
            | TmNOT             Tm 
            | TmBYTE            Tm Tm
            | TmSHL             Tm Tm
            | TmSHR             Tm Tm
            | TmSAR             Tm Tm 
            | TmSHA3            Tm Tm 
            | TmADDRESS         
            | TmBALANCE         Tm 
            | TmORIGIN  
            | TmCALLER
            | TmCALLVALUE
            | TmCALLDATALOAD    Tm
            | TmCALLDATASIZE
            | TmCALLDATACOPY    Tm Tm Tm    -- ()
            | TmCODESIZE     
            | TmCODECOPY        Tm Tm Tm    -- ()
            | TmGASPRICE
            | TmEXTCODESIZE     Tm
            | TmEXTCODESOPY     Tm Tm Tm Tm -- ()
            | TmRETURNDATASIZE
            | TmRETURNDATACOPY  Tm Tm Tm    -- ()
            | TmEXTCODEHASH     Tm 
            | TmBLOCKHASH       Tm
            | TmCOINBASE
            | TmTIMESTAMP
            | TmNUMBER
            | TmDIFFICULTY
            | TmGASLIMIT
            | TmCHAINID
            | TmSELFBALANCE
            | TmPOP             Tm          -- ()
            | TmMLOAD           Tm  
            | TmMSTORE          Tm Tm       -- ()
            | TmMSTORE8         Tm Tm       -- ()
            | TmSLOAD           Tm          
            | TmSSTORE          Tm Tm       -- ()
            | TmJUMP            Tm          -- ()
            | TmJUMPI           Tm Tm       -- ()
            | TmPC
            | TmMSIZE
            | TmGAS
            | TmJUMPDEST                    -- ()
            | TmPUSH1  String 
            | TmPUSH2  String 
            | TmPUSH3  String 
            | TmPUSH4  String 
            | TmPUSH5  String 
            | TmPUSH6  String 
            | TmPUSH7  String 
            | TmPUSH8  String 
            | TmPUSH9  String 
            | TmPUSH10 String 
            | TmPUSH11 String 
            | TmPUSH12 String 
            | TmPUSH13 String 
            | TmPUSH14 String 
            | TmPUSH15 String 
            | TmPUSH16 String 
            | TmPUSH17 String 
            | TmPUSH18 String 
            | TmPUSH19 String 
            | TmPUSH20 String 
            | TmPUSH21 String 
            | TmPUSH22 String 
            | TmPUSH23 String 
            | TmPUSH24 String 
            | TmPUSH25 String 
            | TmPUSH26 String 
            | TmPUSH27 String 
            | TmPUSH28 String 
            | TmPUSH29 String 
            | TmPUSH30 String 
            | TmPUSH31 String 
            | TmPUSH32 String 
            | TmDUP1              
            | TmDUP2            
            | TmDUP3 
            | TmDUP4 
            | TmDUP5 
            | TmDUP6 
            | TmDUP7 
            | TmDUP8 
            | TmDUP9 
            | TmDUP10
            | TmDUP11
            | TmDUP12
            | TmDUP13
            | TmDUP14
            | TmDUP15
            | TmDUP16
            | TmSWAP1                           --()
            | TmSWAP2                           --()
            | TmSWAP3                           --()
            | TmSWAP4                           --()
            | TmSWAP5                           --()
            | TmSWAP6                           --()
            | TmSWAP7                           --()
            | TmSWAP8                           --()
            | TmSWAP9                           --()
            | TmSWAP10                          --()
            | TmSWAP11                          --()
            | TmSWAP12                          --()
            | TmSWAP13                          --()
            | TmSWAP14                          --()
            | TmSWAP15                          --()
            | TmSWAP16                          --()
            | TmLOG0        Tm Tm               --()
            | TmLOG1        Tm Tm Tm            --()
            | TmLOG2        Tm Tm Tm Tm         --()
            | TmLOG3        Tm Tm Tm Tm Tm      --()
            | TmLOG4        Tm Tm Tm Tm Tm Tm   --()
            | TmCREATE      Tm Tm Tm 
            | TmCALL        Tm Tm Tm Tm Tm Tm Tm
            | TmCALLCODE    Tm Tm Tm Tm Tm Tm Tm
            | TmRETURN      Tm Tm
            | TmDELEGATECALL   Tm Tm Tm Tm Tm Tm
            | TmCREATE2     Tm Tm Tm Tm
            | TmSTATICCALL  Tm Tm Tm Tm Tm Tm
            | TmREVERT      Tm Tm 
            | TmINVALID
            | TmSELFDESTRUCT Tm 
            deriving (Show, Eq)


paren' ops = paren (reverse ops) [-2]
 
paren :: [OPCODE] -> [Int] -> [OPCODE] 
paren [EOF] [-2]    = []
paren []   (hd:tl)  = paren [EOF] (hd:tl)
paren asms (hd:tl)  = 
    if hd == -1  then error "arg counting stack cannot be -1" else   
    if hd == 0   then case tl of 
                    [-2]  -> R : paren [EOF] [-2] 
                    t:ts  -> R : paren asms (t-1:ts) else case asms of 
    ADD         : ops -> L : ADD         : paren ops (2:hd:tl)      
    MUL         : ops -> L : MUL         : paren ops (2:hd:tl) 
    SUB         : ops -> L : SUB         : paren ops (2:hd:tl)
    DIV         : ops -> L : SUB         : paren ops (2:hd:tl)
    SDIV        : ops -> L : SDIV        : paren ops (2:hd:tl)
    MOD         : ops -> L : MOD         : paren ops (2:hd:tl)
    SMOD        : ops -> L : SMOD        : paren ops (2:hd:tl)
    ADDMOD      : ops -> L : ADDMOD      : paren ops (2:hd:tl)
    MULMOD      : ops -> L : MULMOD      : paren ops (2:hd:tl)
    EXP         : ops -> L : EXP         : paren ops (2:hd:tl)
    SIGNEXTEND  : ops -> L : SIGNEXTEND  : paren ops (2:hd:tl)
    LT          : ops -> L : LT          : paren ops (2:hd:tl)
    GT          : ops -> L : GT          : paren ops (2:hd:tl)
    SLT         : ops -> L : SLT         : paren ops (2:hd:tl)
    SGT         : ops -> L : SGT         : paren ops (2:hd:tl)
    EQ          : ops -> L : EQ          : paren ops (2:hd:tl)
    ISZERO      : ops -> L : ISZERO      : paren ops (1:hd:tl)
    AND         : ops -> L : AND         : paren ops (2:hd:tl)
    OR          : ops -> L : OR          : paren ops (2:hd:tl)
    XOR         : ops -> L : XOR         : paren ops (2:hd:tl)
    NOT         : ops -> L : NOT         : paren ops (1:hd:tl)
    BYTE        : ops -> L : BYTE        : paren ops (2:hd:tl)
    SHL         : ops -> L : SHL         : paren ops (2:hd:tl)
    SHR         : ops -> L : SHR         : paren ops (2:hd:tl)
    SAR         : ops -> L : SAR         : paren ops (2:hd:tl)
    ADDRESS     : ops -> L : ADDRESS : R : paren ops (hd-1:tl)
    BALANCE     : ops -> L : BALANCE     : paren ops (1:hd:tl)
    ORIGIN      : ops -> L : ORIGIN  : R : paren ops (hd-1:tl)
    CALLER      : ops -> L : CALLER  : R : paren ops (hd-1:tl)
    CALLVALUE   : ops -> L : CALLVALUE:R : paren ops (hd-1:tl)
    CALLDATASIZE: ops -> L : CALLDATASIZE: paren ops (1:hd:tl)
    CALLDATACOPY: ops -> U : AND         : paren ops (2:hd:tl)
    AND         : ops -> L : AND         : paren ops (2:hd:tl)
    AND         : ops -> L : AND         : paren ops (2:hd:tl)
    AND         : ops -> L : AND         : paren ops (2:hd:tl)
    PUSH1  s    : ops -> L : PUSH1  s: R : paren ops (hd-1:tl) 
    PUSH2  s    : ops -> L : PUSH2  s: R : paren ops (hd-1:tl) 
    PUSH3  s    : ops -> L : PUSH3  s: R : paren ops (hd-1:tl) 
    PUSH4  s    : ops -> L : PUSH4  s: R : paren ops (hd-1:tl) 
    PUSH5  s    : ops -> L : PUSH5  s: R : paren ops (hd-1:tl) 
    PUSH6  s    : ops -> L : PUSH6  s: R : paren ops (hd-1:tl) 
    PUSH7  s    : ops -> L : PUSH7  s: R : paren ops (hd-1:tl) 
    PUSH8  s    : ops -> L : PUSH8  s: R : paren ops (hd-1:tl) 
    PUSH9  s    : ops -> L : PUSH9  s: R : paren ops (hd-1:tl) 
    PUSH10 s    : ops -> L : PUSH10 s: R : paren ops (hd-1:tl) 
    PUSH11 s    : ops -> L : PUSH11 s: R : paren ops (hd-1:tl) 
    PUSH12 s    : ops -> L : PUSH12 s: R : paren ops (hd-1:tl) 
    PUSH13 s    : ops -> L : PUSH13 s: R : paren ops (hd-1:tl) 
    PUSH14 s    : ops -> L : PUSH14 s: R : paren ops (hd-1:tl) 
    PUSH15 s    : ops -> L : PUSH15 s: R : paren ops (hd-1:tl) 
    PUSH16 s    : ops -> L : PUSH16 s: R : paren ops (hd-1:tl) 
    PUSH17 s    : ops -> L : PUSH17 s: R : paren ops (hd-1:tl) 
    PUSH18 s    : ops -> L : PUSH18 s: R : paren ops (hd-1:tl) 
    PUSH19 s    : ops -> L : PUSH19 s: R : paren ops (hd-1:tl) 
    PUSH20 s    : ops -> L : PUSH20 s: R : paren ops (hd-1:tl) 
    PUSH21 s    : ops -> L : PUSH21 s: R : paren ops (hd-1:tl) 
    PUSH22 s    : ops -> L : PUSH22 s: R : paren ops (hd-1:tl) 
    PUSH23 s    : ops -> L : PUSH23 s: R : paren ops (hd-1:tl) 
    PUSH24 s    : ops -> L : PUSH24 s: R : paren ops (hd-1:tl) 
    PUSH25 s    : ops -> L : PUSH25 s: R : paren ops (hd-1:tl) 
    PUSH26 s    : ops -> L : PUSH26 s: R : paren ops (hd-1:tl) 
    PUSH27 s    : ops -> L : PUSH27 s: R : paren ops (hd-1:tl) 
    PUSH28 s    : ops -> L : PUSH28 s: R : paren ops (hd-1:tl) 
    PUSH29 s    : ops -> L : PUSH29 s: R : paren ops (hd-1:tl) 
    PUSH30 s    : ops -> L : PUSH30 s: R : paren ops (hd-1:tl) 
    PUSH31 s    : ops -> L : PUSH31 s: R : paren ops (hd-1:tl) 
    PUSH32 s    : ops -> L : PUSH32 s: R : paren ops (hd-1:tl) 
    EOF         : ops -> paren (EOF:ops) (hd:tl) 

prog = 
    [PUSH1 "1", PUSH1 "2", ADD, 
     PUSH1 "3", PUSH1 "4", SUB, 
     ADD, 
     PUSH1 "1", PUSH1 "2", ADD, 
     PUSH1 "3", PUSH1 "4", SUB, 
     MUL, 
     SUB]
prog2 = 
    [PUSH1 "1", PUSH1 "2", ADD, 
     PUSH1 "3", 
     ADD]


splitT :: [OPCODE]  -> (Tree OPCODE, [OPCODE]) 
splitT opcodes = case opcodes of 
    ADD         : ops   -> let (args,cont) = splitF ops in (Node ADD       args, cont)
    SUB         : ops   -> let (args,cont) = splitF ops in (Node SUB       args, cont)
    MUL         : ops   -> let (args,cont) = splitF ops in (Node MUL       args, cont)
    PUSH1  s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH1  s) ret, cont)
    PUSH2  s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH2  s) ret, cont)
    PUSH3  s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH3  s) ret, cont)
    PUSH4  s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH4  s) ret, cont)
    PUSH5  s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH5  s) ret, cont)
    PUSH6  s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH6  s) ret, cont)
    PUSH7  s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH7  s) ret, cont)
    PUSH8  s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH8  s) ret, cont)
    PUSH9  s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH9  s) ret, cont)
    PUSH10 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH10 s) ret, cont)
    PUSH11 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH11 s) ret, cont)
    PUSH12 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH12 s) ret, cont)
    PUSH13 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH13 s) ret, cont)
    PUSH14 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH14 s) ret, cont)
    PUSH15 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH15 s) ret, cont)
    PUSH16 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH16 s) ret, cont)
    PUSH17 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH17 s) ret, cont)
    PUSH18 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH18 s) ret, cont)
    PUSH19 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH19 s) ret, cont)
    PUSH20 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH20 s) ret, cont)
    PUSH21 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH21 s) ret, cont)
    PUSH22 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH22 s) ret, cont)
    PUSH23 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH23 s) ret, cont)
    PUSH24 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH24 s) ret, cont)
    PUSH25 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH25 s) ret, cont)
    PUSH26 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH26 s) ret, cont)
    PUSH27 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH27 s) ret, cont)
    PUSH28 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH28 s) ret, cont)
    PUSH29 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH29 s) ret, cont)
    PUSH30 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH30 s) ret, cont)
    PUSH31 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH31 s) ret, cont)
    PUSH32 s    : ops   -> let (ret,cont)  = splitF ops in (Node (PUSH32 s) ret, cont)

splitF opcodes = case opcodes of 
    L : ops         -> let (ret,cont) = splitT ops in 
                       let (rest,cont') = splitF cont in (ret:rest, cont') 
    R : ops         -> ([], ops) 
    []              -> ([], []) 

    

    
    


