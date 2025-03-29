module BODY2BD where 

import AST
import Term 
import Tree
import Type 



body2bd :: BODY -> BD 
body2bd (BODY p1 ds t p2) = BD p1 (seq2term ds t) p2 


seq2term :: [Decl] -> Term -> Term 
seq2term [] tm = tm 
seq2term (d:ds) tm = case d of 
    FLET i ps t maybeP  -> 
        RED TmAPP [RED (TmLAM i Untyped) [seq2term ds tm],  
                   RED (TmFIX i (fst <$> ps) Untyped) (t:p) ] 
                   where 
                    p = case maybeP of 
                        Nothing -> [] 
                        Just p  -> [BLK (Predicate p)[]] 
    SLET i t maybeP     -> 
        undefined 


