module Eval where 


import AST
import Term
import Type
import Tree
import Bind 
import Typing 
import Utils






eval :: Ctx -> Ctx -> Term -> Term 
eval ctx stx tr = tr 









processCN :: CONTRACT -> CONTRACT 
processCN cn = (fst3 . reconCN [] [] 0) cn    

--typingTest :: CONTRACT -> (CONTRACT, Ctx, Ctx, UVar, Constraint) 
-- typingTest cn = processCN [] [] 0 [] cn  

typingTest2 :: CONTRACT -> (CONTRACT, UVar, Constraint) 
typingTest2 cn = reconCN [] [] 0  cn 
typing = fst3 . typingTest2
