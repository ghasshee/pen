module  Predicate where 

import Term 

type Formulae = STFormulae 

data STFormulae = FTrue
                | FAtom AFormulae 
                | FAnd STFormulae STFormulae
                | FNot STFormulae 
                | E PathFormulae 
                | A PathFormulae
                deriving (Eq, Read) 

instance Show STFormulae where 
    show (FAtom   a )   = show a 
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

data AFormulae  = AEq AST AST
                | AGt AST AST
                | ALt AST AST 
                | AGe AST AST 
                | ALe AST AST 
                deriving (Eq, Read) 
                
instance Show AFormulae where 
    show (AEq a b   )   = show a ++ "==" ++ show b
    show (AGt a b   )   = show a ++ ">"  ++ show b 
    show (ALt a b   )   = show a ++ "<"  ++ show b 
    show (AGe a b   )   = show a ++ ">=" ++ show b 
    show (ALe a b   )   = show a ++ "<=" ++ show b 


