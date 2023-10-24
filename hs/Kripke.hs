module Kripke where 


import Set 
import Predicate
import PG 



data Kripke s atom = K [s] [s] [(s,s)] (s -> [atom])   



pg2kripke :: PG -> Kripke Int Formulae
pg2kripke = undefined 

