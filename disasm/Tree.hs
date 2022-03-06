module Tree where 

import Comonad

data Tree a' = Node a' [Tree a'] 


instance Show a' => Show (Tree a') where
    show    = showT "   "
        where 
        showT s (Node a x)      = "+- " ++ show a ++ "\n" ++ showF s x
        showF s []              = ""
        showF s [Node a x]      = s ++ "+- " ++ show a ++ "\n" ++ showF(s ++ "   ")x
        showF s (Node a x:xs)   = s ++ "+- " ++ show a ++ "\n" ++ showF(s ++ "|  ")x
                                    ++ showF s xs 

foldt g h d c (Node a [])       = g a d 
foldt g h d c (Node a xs)       = g a (foldf g h d c xs)
foldf g h d c []                = c 
foldf g h d c (x:xs)            = h (foldt g h d c x) (foldf g h d c xs)

instance Functor Tree where 
    fmap f (Node a [])          = Node (f a) []
    fmap f (Node a (x:xs))      = Node (f a) (fmap f x:fmap(fmap f)xs)

instance Comonad Tree where 
    extract  (Node a _ )        = a 
    extend f (Node a [])        = Node (f (Node a []))      [] 
    extend f (Node a(x:xs))     = Node (f (Node a(x:xs))) (fmap (extend f) (x:xs))

{-- e.g. --} 
tt = Node 1
        ( 
            Node 4 ((Node 6 []) : Node 7 [] : []) : 
            Node 5 [] : 
            []
        )

sum_tree = foldt (+)(+) 0 0 


