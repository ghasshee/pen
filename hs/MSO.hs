module MSO where 

import Set 
import Data.List (sort) 
import Prelude hiding (any, showList)
import Opcode
import Automata

data Fst    = F Int 
            | Loc Int 
            deriving (Eq)
data Snd    = S Int 
            | Set [Fst] deriving (Eq)

data Any a  = Except [a] 
            | Select [a] 

instance Show a => Show (Any a) where 
    show (Select [])  = "∅"
    show (Except [])  = "*"
    show (Select l )  = "{" ++ showList l ++ "}" 
    show (Except l)   = "*\\{" ++ showList l ++ "}" 

showList []   = ""
showList [a]  = show a 
showList (a:as) = show a ++ "," ++  showList as 

any     :: forall a. Any a 
any     = Except [] 
none    :: forall a. Any a
none    = Select [] 
jst a   = Select [a]

instance Ord a => Eq (Any a) where 
    Except []       == Except []    = True
    Except (a:as)   == Except l     = if a `elem` l 
                                        then Except as == Except(filter(/=a)l) 
                                        else False
    Select []       == Select []    = True
    Select (a:as)   == Select l     = if a `elem` l 
                                        then Select as == Select(filter(/=a)l) 
                                        else False
    _               == _            = False  


anyminus :: Ord a => Any a -> Any a -> Any a 
anyminus (Select [])    _               = none
anyminus _              (Except [])     = none
anyminus a              (Select [])     = a 
anyminus (Select l)     (Select(a:as))  = if a `elem` l 
                                            then anyminus (Select(filter(/=a)l))(Select as)
                                            else anyminus (Select l) (Select as)
anyminus (Except l)     (Select(a:as))  = if a `elem` l 
                                            then anyminus (Except l) (Select as) 
                                            else anyminus (Except (a:l)) (Select as)  
anyminus (Except(a:as)) (Except l)      = if a `elem` l 
                                            then anyminus (Except as)(Except l) 
                                            else anyminus (Except as)(Except (a:l)) 
anyminus (Select(b:bs)) (Except l)      = if b `elem` l 
                                            then Select (b:bs') 
                                            else Select bs' 
                                            where  
                                                Select bs' = anyminus(Select bs)(Except l)


instance Ord a => Ord (Any a) where 
    Select []   <= _                  = True
    _           <= Except []          = True
    Except []   <= a                  = Except [] == a 
    a           <= Select []          = a == Select [] 
    Select(a:as)<= Select l           = if a `elem` l then Select as <= Select l else False
    Select(a:as)<= Except l           = if a `elem` l then False else Select as <= Except l
    Except(a:as)<= Select l           = if a `elem` l 
                                            then Except as <= Select l 
                                            else Except as <= Select (a:l)
    Except(a:as)<= Except l           = if a `elem` l 
                                            then Except as <= Except (filter(/=a)l)
                                            else Except as <= Except l 


data MSO a  = Lab a Fst
            | Suc Int Fst Fst
            | Le Fst Fst
            | Elem Fst Snd
            | Eq Fst Fst
            | Bot
            | Or (MSO a) (MSO a)
            | Not (MSO a) 
            | Ex1 Fst (MSO a)
            | Ex2 Snd (MSO a) 




-- fv returns bounded variables and free variables 
fv :: MSO a -> ([Either Fst Snd], [Either Fst Snd])
fv m = loop m [] [] where 
    loop (Lab a i)      c l     = (c,l) 
    loop (Suc i x1 x2)  c l     = (c,addsetifnot2 c l (Left x1) (Left x2))
    loop (Le x1 x2)     c l     = (c,addsetifnot2 c l (Left x1) (Left x2))
    loop (Elem x y)     c l     = (c,addsetifnot2 c l (Left x ) (Right y))
    loop (Eq x1 x2)     c l     = (c,addsetifnot2 c l (Left x1) (Left x2))
    loop (Bot     )     c l     = (c,l) 
    loop (Or m1 m2)     c l     = loop m2 c' l' where 
        (c',l') = loop m1 c l
    loop (Not m   )     c l     = loop m c l 
    loop (Ex1 x m )     c l     = loop m (Left x:c) l 
    loop (Ex2 y m )     c l     = loop m (Right y:c) l 

----------------------
-- MSO -> Automata  --
----------------------

data Bit = T_F Int
         | F_F Int
         | A_F Int
         | T_S Int
         | F_S Int
         | A_S Int deriving (Eq) 

instance Show Bit where 
    show (T_F i) = "1_x" ++ show i
    show (F_F i) = "0_x" ++ show i
    show (A_F i) = "*_x" ++ show i 
    show (T_S i) = "1_X" ++ show i
    show (F_S i) = "0_X" ++ show i
    show (A_S i) = "*_X" ++ show i 

instance Ord Bit where 
    T_F i <= A_F j = i == j
    F_F i <= A_F j = i == j
    T_S i <= A_S j = i == j
    F_S i <= A_S j = i == j
    b     <= b'    = b == b'  
{--
instance Eq Bit where 
    T_F i == T_F j  = i==j
    F_F i == F_F j  = i==j
    A_F i == T_F j  = i==j
    A_F i == F_F j  = i==j
    T_F i == A_F j  = i==j
    F_F i == A_F j  = i==j
    _     == _      = False
--}

instance {-# OVERLAPPING #-} Ord [Bit] where 
    []      <= _    = True
    (b:bs)  <= bs'  = if b `elem` bs' then bs <= bs' else False
    

convert :: (Eq a, Ord a) => MSO a -> Automata Int (Any a, [Bit])  
convert m = 
    let (cl,op) = fv m in loop m where 
        loop (Lab a (F i)) =
            A [Q 1,Q 2] [] 
                [(Q 1,(any  ,[A_F i]),Q 1)
                ,(Q 1,(jst a,[T_F i]),Q 2)
                ,(Q 2,(any  ,[A_F i]),Q 2)]
                (Q 1) [Q 2] 
        loop (Suc k (F i) (F j)) = 
            A [Q 1, Q 2, Q 3] [] 
                [(Q 1,(any,[A_F i,A_F j]),Q 1)
                ,(Q 1,(any,[T_F i,F_F j]),Q 2)
                ,(Q 2,(any,[F_F i,T_F j]),Q 3)
                ,(Q 3,(any,[A_F i,A_F j]),Q 3)] 
                (Q 1) [Q 3] 
        loop (Elem (F i) (S j)) = 
            A [Q 1, Q 2] [] 
                [(Q 1,(any,[A_F i,A_S j]),Q 1)
                ,(Q 1,(any,[T_F i,T_S j]),Q 1)
                ,(Q 2,(any,[A_F i,A_S j]),Q 2)] (Q 1) [Q 2]
        loop (Not m)    = 
            A qs as trs i (setminus qs t) where 
                A qs as trs i t = loop m 
        loop (Or m1 m2) = 
            let fvs1    = snd $ fv m1 in 
            let fvs2    = snd $ fv m2 in 
            A   [P(q,q') | q <- qs , q' <- qs'] []
                (
                [(P(p,p'),(a ,l ),P(q,q')) | (p,(a,l),q) <- trs, (p',(a',l'),q') <- trs', a <= a', l <= l' ] ++
                [(P(p,p'),(a',l'),P(q,q')) | (p,(a,l),q) <- trs, (p',(a',l'),q') <- trs', a'<= a , l'<= l  ]
                )
                (P(i,i')) [P(t1,t2) | t1 <- t , t2 <- t'] where 
                    A qs  as  trs  i  t  = loop m1 
                    A qs' as' trs' i' t' = loop m2
        loop (Ex1 (F k) m)  = 
            let A qs as trs i t = loop m in 
            A qs as (rmVar1 (F k) trs) i t 
        loop (Ex2 (S k) m)  = 
            let A qs as trs i t = loop m in 
            A qs as (rmVar2 (S k) trs) i t 

rmVar1 (F i) []                    = [] 
rmVar1 (F i) ((q,(a,vs),q'):trs)   = (q,(a, loop i vs),q') : rmVar1 (F i) trs
    where 
        loop i (F_F j:vs) | i == j   =     loop i vs
        loop i (T_F j:vs) | i == j   =     loop i vs
        loop i (A_F j:vs) | i == j   =     loop i vs
        loop i (v    :vs)            = v : loop i vs 
        loop i []                    = []

rmVar2 (S i) []                    = [] 
rmVar2 (S i) ((q,(a,vs),q'):trs)   = (q,(a, loop i vs),q') : rmVar2 (S i) trs 
    where 
        loop i (F_S j:vs) | i == j   =     loop i vs
        loop i (T_S j:vs) | i == j   =     loop i vs
        loop i (A_S j:vs) | i == j   =     loop i vs
        loop i (v    :vs)            = v : loop i vs 
        loop i []                    = []


-- e.g. 

eg = Ex1 (F 1) (Or (Lab 'a' (F 1)) (Lab 'b' (F 1)) )  
a  = convert eg
eg'= runAutomata a [(jst 'a',[])]
