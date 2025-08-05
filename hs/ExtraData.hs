{-# LANGUAGE BinaryLiterals #-} 


module ExtraData where 



import Datatype




-- 2 bit header : type of extraData 
-- 0b00 : Creation Code 
-- 0b01 : Datatype Definition 
-- 0b10 : --
-- 0b11 : Other Byte Data 


data ExtraData  = EDCr      -- Creation Code 
                | EDDT      -- Datatype Def 
                | EDOther   -- Other Byte Data

data DataStructure  = Pointer Int
                    | Undefnd  

ed2binary EDCr      = "00"
ed2binary EDDT      = "01" 
ed2binary EDOther   = "11"



data EDDatatype = Undefined 










