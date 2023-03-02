-- Guarded Commands with Blocks 
-- see p.93 of Formal Methods 


module GCB where 

import GCLL

data Bind   = Var String 
            | FVar String Int Node Node -- Name NumArgs InitNode LastNode

data D  = DVar Int
        | DSto Int
        | DSeq D D 
        | DProc String [Int] Int C  -- Name Args Ret Body

data C  = CBlk D C 
        | CAsgn Int A
        | CSkip 
        | CSeq C C 
        | CIf GC 
        | CDo GC 
        | CCall String [Int] Int 

data GC = GCond B C
        | GSeq GC GC 


type A  = EXPR 
type B  = EXPR 

type NewNode    = Int
type NewVar     = Int 
type NewSto     = Int 



