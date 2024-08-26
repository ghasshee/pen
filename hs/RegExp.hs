module RegExp where 

import Semiring 



data KleeneAlgExp a = Var a 
                    | Or   (KleeneAlgExp a)(KleeneAlgExp a) 
                    | Seq  (KleeneAlgExp a)(KleeneAlgExp a) 
                    | Star (KleeneAlgExp a)
                    | None      -- 0 
                    | Empty     -- 1 

instance Show a => Show (KleeneAlgExp a) where 
    showsPrec d (Var a)   = showParen(d>10)(shows a) 
    showsPrec d (Empty)   = showParen(d>10)(showString "ε") 
    showsPrec d (None )   = showParen(d>10)(showString "0") 
    showsPrec d (Star x)  = showParen(d> 9)(showsPrec 9 x . showString "*")
    showsPrec d (x`Or` y) = showParen(d> 6)(showsPrec 6 x . showString "|" . showsPrec 6 y)
    showsPrec d (x`Seq`y) = showParen(d> 7)(showsPrec 7 x . showsPrec 7 y) 

newtype RegExp a = RegExp (KleeneAlgExp a)

regexp :: a -> RegExp a 
regexp a = RegExp (Var a) 
