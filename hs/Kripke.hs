module Kripke {-# WARNING "Kripke Semantics To Be Implemented after Inductive Data Type .." #-} where 


import Set 
import Term
import PG 



data Kripke s atom = K [s] [s] [(s,s)] (s -> [atom])   



pg2kripke   :: PG -> Kripke Int Formulae
pg2kripke   = undefined 
