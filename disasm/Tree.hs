module Tree where 

import Comonad

data Tree a' = Node a' [Tree a'] deriving Eq

data RBTree a' = RED a' [RBTree a'] 
               | BLK a' [RBTree a'] deriving Eq 

instance Show a' => Show (RBTree a') where 
    show    = showT "    "
        where
        showT s (RED a x)       = "+-  " ++ show a ++ "\n" ++ showF s x 
        showT s (BLK a x)       = "*-( " ++ show a ++ "\n" ++ showF s x
        showF s []              = ""
        showF s [RED a x]       = s ++ "+-  " ++ show a ++ "\n" ++ showF(s ++ "    ")x
        showF s [BLK a x]       = s ++ "*-( " ++ show a ++ "\n" ++ showF(s ++ "    ")x
        showF s (RED a x:xs)    = s ++ "+-  " ++ show a ++ "\n" ++ showF(s ++ "|   ")x
                                    ++ showF s xs 
        showF s (BLK a x:xs)    = s ++ "*-( " ++ show a ++ "\n" ++ showF(s ++ "|   ")x
                                    ++ showF s xs 

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


foldrbt g h d c (RED a [])      = g a d 
foldrbt g h d c (BLK a [])      = g a d 
foldrbt g h d c (RED a xs)      = g a (foldrbf g h d c xs)
foldrbt g h d c (BLK a xs)      = g a (foldrbf g h d c xs)
foldrbf g h d c []              = c 
foldrbf g h d c (x:xs)          = h (foldrbt g h d c x) (foldrbf g h d c xs)

instance Functor RBTree where   
    fmap f (RED a [])           = RED (f a) []
    fmap f (BLK a [])           = BLK (f a) []
    fmap f (RED a (x:xs))       = RED (f a) (fmap f x:fmap(fmap f)xs)
    fmap f (BLK a (x:xs))       = BLK (f a) (fmap f x:fmap(fmap f)xs)

instance Functor Tree where 
    fmap f (Node a [])          = Node (f a) []
    fmap f (Node a (x:xs))      = Node (f a) (fmap f x:fmap(fmap f)xs)


instance Comonad RBTree where 
    extract  (RED a _)          = a 
    extract  (BLK a _)          = a 
    extend f (RED a [])         = RED (f (RED a [])) []
    extend f (BLK a [])         = BLK (f (BLK a [])) []
    extend f (RED a (x:xs))     = RED (f (RED a (x:xs))) (fmap (extend f) (x:xs))
    extend f (BLK a (x:xs))     = BLK (f (BLK a (x:xs))) (fmap (extend f) (x:xs))

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


