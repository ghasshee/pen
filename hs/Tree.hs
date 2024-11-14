module Tree where 

import Comonad

data Tree a     = Node a [Tree a]     deriving (Eq,Read)

data RBTree a   = RED a [RBTree a] 
                | BLK a [RBTree a]    deriving (Eq,Read) 

data ROOT a     = RT Int [RBLTree a] deriving (Show, Eq, Read) 

data RBLTree a  = RD  a [RBLTree a] 
                | BK  a [RBLTree a] 
                | LN    (ROOT    a)  deriving (Eq,Read)  

instance Show a => Show (RBLTree a) where 
    show  s  = "\n" ++ showF "    " [s] where
        showF s []              = ""
        showF s [RD a x]        = s ++ "+-  " ++ show a ++ "\n" ++ showF(s ++ "    ")x
        showF s [BK a x]        = s ++ "*-( " ++ show a ++ "\n" ++ showF(s ++ "    ")x
        showF s (RD a x:xs)     = s ++ "+-  " ++ show a ++ "\n" ++ showF(s ++ "|   ")x 
                                    ++ showF s xs 
        showF s (BK a x:xs)     = s ++ "*-( " ++ show a ++ "\n" ++ showF(s ++ "|   ")x 
                                    ++ showF s xs 
        showF s (LN(RT n _):xs) = s ++ "L=> " ++ show n ++ "\n" 
                                    ++ showF s xs 

instance Show a => Show (RBTree a) where 
    show  s  = "\n" ++ showF "    " [s] where
        showF s []              = ""
        showF s [RED a x]       = s ++ "+-  " ++ show a ++ "\n" ++ showF(s ++ "    ")x
        showF s [BLK a x]       = s ++ "*-( " ++ show a ++ "\n" ++ showF(s ++ "    ")x
        showF s (RED a x:xs)    = s ++ "+-  " ++ show a ++ "\n" ++ showF(s ++ "|   ")x
                                    ++ showF s xs 
        showF s (BLK a x:xs)    = s ++ "*-( " ++ show a ++ "\n" ++ showF(s ++ "|   ")x
                                    ++ showF s xs 

instance Show a => Show (Tree a) where
    show t  = showF "   " [t]  where 
        showF s []              = ""
        showF s [Node a x]      = s ++ "+- " ++ show a ++ "\n" ++ showF(s ++ "   ")x
        showF s (Node a x:xs)   = s ++ "+- " ++ show a ++ "\n" ++ showF(s ++ "|  ")x
                                    ++ showF s xs 



foldt g h d c (Node a [])       = g a d 
foldt g h d c (Node a xs)       = g a (foldf g h d c xs)
foldf g h d c []                = c 
foldf g h d c (x:xs)            = h (foldt g h d c x) (foldf g h d c xs)

foldtr                          = foldt 

foldtl g h d c (Node a [])      = g a d
foldtl g h d c (Node a xs)      = g a (foldfl g h d c xs) 
foldfl g h d c []               = c 
foldfl g h d c (x:xs)           = foldfl g h d (h (foldt g h d c x) c) xs 


foldrbt gR gB h d c (RED a [])      = gR a d 
foldrbt gR gB h d c (BLK a [])      = gB a d 
foldrbt gR gB h d c (RED a xs)      = gR a (foldrbf gR gB h d c xs)
foldrbt gR gB h d c (BLK a xs)      = gB a (foldrbf gR gB h d c xs)
foldrbf gR gB h d c []              = c 
foldrbf gR gB h d c (x:xs)          = h (foldrbt gR gB h d c x) (foldrbf gR gB h d c xs)

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
    walkf (x:xs)      uncles        = walkt x uncles : case x of 
                                            RED a _ -> walkf xs (a:uncles) 
                                            BLK a _ -> walkf xs uncles 
    

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


