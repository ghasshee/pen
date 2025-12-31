module Fix where 









data Fix f = In (f (Fix f))






foldMu :: Functor f => (f a -> a) -> Fix f -> a 
foldMu = cata 

--ana = unfoldMu 

unfoldMu :: Functor f => (a -> f a) -> a -> Fix f 
unfoldMu coalg a = In (unfoldMu coalg <$> coalg a) 

cata :: Functor f => (f a -> a) -> Fix f -> a 
cata alg (In fx) = alg (cata alg <$> fx) 

showFix :: Functor f => (f String -> String) -> Fix f -> String
showFix = cata  

show' :: String -> String 
show' "" = "" 
show' ('\\':  xs) = show' xs 
show' ('"' :  xs) = show' xs 
show' (x   :  xs) =  x : show' xs 






-- e.g. List 

instance Show a => Show (Fix (ListF a)) where 
    show = show' . showFix show 

data ListF a x = NilF 
               | ConsF a x 
    deriving (Show) 

instance Functor (ListF a) where 
    fmap _ NilF         = NilF 
    fmap f (ConsF a x)  = ConsF a (f x) 

type List a = Fix (ListF a) 

a :: Num a => List a   
a = In (ConsF 1 (In (ConsF 2 (In (ConsF 3 (In NilF))))))


toList (In NilF) = [] 
toList (In (ConsF a as)) = a : toList as 


