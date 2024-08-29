module Logic where 


infixr 6 &&$ 
infixr 6 ||$

(&&$) f g v = (&&) (f v) (g v) 
(||$) f g v = (||) (f v) (g v)


infixr 1 ¬
(¬) a   = not a 
