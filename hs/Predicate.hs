module  Predicate where 

import Term 

data STFormulae = FTrue
                | FAtomic Atomic 
                | FAnd STFormulae STFormulae
                | FNot STFormulae 
                | E PathFormulae 
                | A PathFormulae
                deriving (Eq, Read) 

instance Show STFormulae where 
    show (FAtomic a )   = show a 
    show (FTrue     )   = "⊤"   
    show (FAnd a b  )   = show a ++ "∧" ++ show b
    show (FNot a    )   = "¬" ++ show a 
    show (E pa      )   = "E(" ++ show pa ++ ")"
    show (A pa      )   = "A(" ++ show pa ++ ")" 

data PathFormulae 
                = X STFormulae
                | F STFormulae
                | G STFormulae
                | Union STFormulae STFormulae 
                deriving (Eq, Read) 

instance Show PathFormulae where 
    show (X a       )   = "X(" ++ show a ++ ")"
    show (F a       )   = "F(" ++ show a ++ ")" 
    show (G a       )   = "G(" ++ show a ++ ")" 
    show (Union a b )   = show a ++ "∪" ++ show b

data Atomic     = AEq AST AST
                | AGt AST AST
                | ALt AST AST 
                deriving (Eq, Read) 
                
instance Show Atomic where 
    show (AEq a b   )   = show a ++ "==" ++ show b
    show (AGt a b   )   = show a ++ ">"  ++ show b 
    show (ALt a b   )   = show a ++ "<"  ++ show b 
