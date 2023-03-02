module  Predicate where 

import Term 

type Formulae = STFormulae 

-- state formulae 
data STFormulae = FTrue
                | FARROW STFormulae STFormulae 
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


-- path formulae 
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

-- atomic formulae 
data AFormulae  = AEq Term Term
                | AGt Term Term
                | ALt Term Term 
                | AGe Term Term 
                | ALe Term Term 
                deriving (Eq, Read) 
                
instance Show AFormulae where 
    show (AEq a b   )   = show' a ++ "==" ++ show' b
    show (AGt a b   )   = show' a ++ ">"  ++ show' b 
    show (ALt a b   )   = show' a ++ "<"  ++ show' b 
    show (AGe a b   )   = show' a ++ ">=" ++ show' b 
    show (ALe a b   )   = show' a ++ "<=" ++ show' b 


