module Term where 






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

