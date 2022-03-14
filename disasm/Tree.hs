module Tree where 

import Comonad

data Tree a'    = Node a' [Tree a']     deriving Eq

data RBTree a'  = RED a' [RBTree a'] 
                | BLK a' [RBTree a']    deriving Eq 


data ROOT a'    = RT Int [RBLTree a'] deriving (Show, Eq) 

data RBLTree a' = RD  a' [RBLTree a'] 
                | BK  a' [RBLTree a'] 
                | LN     (ROOT   a')  deriving Eq  

instance Show a' => Show (RBLTree a') where 
    show  s  = "\n" ++ showT "    " s
        where
        showT s (RD a x)        = "+-  " ++ show a ++ "\n" ++ showF s x 
        showT s (BK a x)        = "*-( " ++ show a ++ "\n" ++ showF s x
        showT s (LN(RT n _))    = "*=> LINK" ++ show n
        showF s []              = ""
        showF s [RD a x]        = s ++ "+-  " ++ show a ++ "\n" ++ showF(s ++ "    ")x
        showF s [BK a x]        = s ++ "*-( " ++ show a ++ "\n" ++ showF(s ++ "    ")x
        showF s [LN(RT n _)]    = s ++ "*=> LINK " ++ show n ++ "\n" 
        showF s (RD a x:xs)     = s ++ "+-  " ++ show a ++ "\n" 
                                    ++ showF(s ++ "|   ")x ++ showF s xs 
        showF s (BK a x:xs)     = s ++ "*-( " ++ show a ++ "\n" 
                                    ++ showF(s ++ "|   ")x ++ showF s xs 
        showF s (LN(RT n _):xs) = s ++ "*=> LINK " ++ show n ++ "\n" 
                                    ++ showF s xs 

instance Show a' => Show (RBTree a') where 
    show  s  = "\n" ++ showT "    " s
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

foldrblt g h d c (RD a [])      = g a d 
foldrblt g h d c (BK a [])      = g a d 
foldrblt g h d c (RD a xs)      = g a (foldrblf g h d c xs)
foldrblt g h d c (BK a xs)      = g a (foldrblf g h d c xs)
foldrblf g h d c []             = c 
foldrblf g h d c (x:xs)         = h (foldrblt g h d c x) (foldrblf g h d c xs)


ancesterRB = flip walkrbt [] where 
    walkrbt (RED  a []) parents     = RED  (a,parents) []
    walkrbt (RED  a xs) parents     = RED  (a,parents) (walkrbf xs (a:parents))
    walkrbt (BLK  a []) parents     = BLK  (a,parents) []
    walkrbt (BLK  a xs) parents     = BLK  (a,parents) (walkrbf xs (a:parents))
    walkrbf []          parents     = []
    walkrbf (x:xs)      parents     = walkrbt x parents : walkrbf xs parents 

ancester = flip walkt [] where 
    walkt (Node a []) parents       = Node (a,parents) []
    walkt (Node a xs) parents       = Node (a,parents) (walkf xs (a:parents))
    walkf []          parents       = []
    walkf (x:xs)      parents       = walkt x parents : walkf xs parents 

elder_uncles = flip walkt [] where 
    walkt (Node a []) uncles        = Node (a,uncles) [] 
    walkt (Node a xs) uncles        = Node (a,uncles) (walkf xs []) 
    walkf []          uncles        = []
    walkf (x:xs)      uncles        = walkt x uncles : walkf xs (x:uncles) 

elder_unclesRB = flip walkt [] where
    walkt (RED  a []) uncles        = RED  (a,uncles) [] 
    walkt (BLK  a []) uncles        = BLK  (a,uncles) [] 
    walkt (RED  a xs) uncles        = RED  (a,uncles) (walkf xs uncles) 
    walkt (BLK  a xs) uncles        = BLK  (a,uncles) (walkf xs uncles) 
    walkf []          uncles        = []
    walkf (x:xs)      uncles        = walkt x uncles : walkf xs (a:uncles) 
                                        where a = case x of 
                                                    RED a _ -> a 
                                                    BLK a _ -> a 
    

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


