module ReduceSto where 

import GCLL
import Type
import Term
import Tree
import AST


reduceSTOTm reduce tm = case tm of 
    TmSTO n         -> TmSTO (n-reduce)   
    tm              -> tm 

reduceSTOTerm reduce tm = foldrbt reduce_gR reduce_gB (:) [] [] tm  where 
    reduce_gR a d = RED (reduce a) d 
    reduce_gB a d = RED (reduce a) d 

reduceSTO reduce cn = foldCONTRACT_Tm (reduceSTOTerm reduce) cn  







