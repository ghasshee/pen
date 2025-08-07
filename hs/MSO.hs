
module MSO {-# WARNING "Only Toy MSO Logic implemented" #-} where 


import Logic 
import Set 
import Any
import Data.List (sort) 
import Prelude hiding (any) 
import Opcode
import Node 
import Edge
import Automata






data Fst    = F Int 
            | Loc Int 
            deriving (Show, Eq)
data Snd    = S Int 
            | Set [Fst] deriving (Show, Eq)


data MSO a  = Lab a Fst             -- Lab a x   : The location pointed by location variable x::Fst of the stream is occpied with a 
            | Suc Int Fst Fst       -- Suc _ x y : The location of y is next to that of x 
            | Le Fst Fst            -- Le x y    : The location of x is smaller than that of y 
            | Elem Fst Snd          -- Elem x X  : The set X contains location variable x or the location of x 
            | Eq Fst Fst            -- Eq x y    : The location of x is equal to that of y 
            | Bot
            | Or (MSO a) (MSO a)
            | Not (MSO a) 
            | Ex1 Fst (MSO a)
            | Ex2 Snd (MSO a) 
            deriving (Show, Eq) 




-- fv returns bounded variables and free variables 
fv :: MSO a -> ([Either Fst Snd], [Either Fst Snd])
fv m = loop m [] [] where 
    loop (Lab a i)      c l     = (c,l) 
    loop (Suc i x1 x2)  c l     = (c,addifnot2 c l (Left x1) (Left x2))
    loop (Le x1 x2)     c l     = (c,addifnot2 c l (Left x1) (Left x2))
    loop (Elem x y)     c l     = (c,addifnot2 c l (Left x ) (Right y))
    loop (Eq x1 x2)     c l     = (c,addifnot2 c l (Left x1) (Left x2))
    loop (Bot     )     c l     = (c,l) 
    loop (Or m1 m2)     c l     = loop m2 c' l' where 
        (c',l') = loop m1 c l
    loop (Not m   )     c l     = loop m c l 
    loop (Ex1 x m )     c l     = loop m (Left x:c) l 
    loop (Ex2 y m )     c l     = loop m (Right y:c) l 





----------------------
-- MSO -> Automata  --
----------------------

data AnyBit var = O_ var | I_ var | A_ var deriving (Eq) 

instance Show var => Show (AnyBit var) where 
    show (O_ a) = "0_" ++ show a 
    show (I_ a) = "1_" ++ show a 
    show (A_ a) = "*_" ++ show a 
    
instance {-# Overlapping #-} Eq a => Ord [AnyBit a] where 
    []      <= _     = True 
    (b:bs)  <= bs'   = if b `elem` bs' then bs <= bs' else False 
    

data Bit = I_x Int      -- True  FirstVariable 
         | O_x Int      -- False FirstVariable
         | A_x Int      -- Any   FirstVariable
         | I_X Int      -- True  SecondVariable
         | O_X Int      -- False SecondVariable
         | A_X Int      -- Any   SecondVariable
         deriving (Eq) 

fst_var b = case b of 
    I_x _ -> True 
    O_x _ -> True
    A_x _ -> True
    _     -> False

var_num b = case b of 
    I_x i -> i
    O_x i -> i 
    A_x i -> i 
    I_X i -> i
    O_X i -> i 
    A_X i -> i 

instance Show Bit where 
    show (I_x i) = "1_x" ++ show i
    show (O_x i) = "0_x" ++ show i
    show (A_x i) = "*_x" ++ show i 
    show (I_X i) = "1_X" ++ show i
    show (O_X i) = "0_X" ++ show i
    show (A_X i) = "*_X" ++ show i 


instance {-# OVERLAPPING #-} Ord [Bit] where 
    []      <= _    = True
    (b:bs)  <= bs'  = if b `elem` bs' then bs <= bs' else False
    

convertMSO :: (Eq a, Ord a) => MSO a -> Automata (Node Int)(Any a, [Bit])  
convertMSO m = 
    let (cl,op) = fv m in loop [] m where 
        loop ctx (Lab a (F i))      = A [Q 1,Q 2] [] es [Q 1] [Q 2] where 
            es =    [(Q 1,(any  ,[A_x i]),Q 1)
                    ,(Q 1,(jst a,[I_x i]),Q 2)
                    ,(Q 2,(any  ,[A_x i]),Q 2)]
        loop ctx (Lab a (Loc n))    = A qs [] es [Q 1] [Q (n+1)] where 
            qs =    Q <$> [1 .. n+1] 
            es =    [(Q n, (jst a,[]), Q(n+1))
                    ,(Q(n+1),(any,[]), Q(n+1))] 
                ++  ((\k -> (Q k,(any , []),Q(k+1))) <$> [1..(n-1)] )
        loop ctx (Suc k (F i)(F j)) = A [Q 1,Q 2,Q 3] [] es [Q 1] [Q 3] where 
            es =    [(Q 1,(any,[A_x i,A_x j]),Q 1)
                    ,(Q 1,(any,[I_x i,O_x j]),Q 2)
                    ,(Q 2,(any,[O_x i,I_x j]),Q 3)
                    ,(Q 3,(any,[A_x i,A_x j]),Q 3)] 
        loop ctx (Elem (F i) (S j)) = A [Q 1,Q 2] [] es [Q 1] [Q 2] where 
            es =    [(Q 1,(any,[A_x i,A_X j]),Q 1)
                    ,(Q 1,(any,[I_x i,I_X j]),Q 1)
                    ,(Q 2,(any,[A_x i,A_X j]),Q 2)] 
        loop ctx (Eq (F i) (Loc n)) =  undefined 
        loop ctx (Not m)            = A qs as trs i (qs \\ t) where 
                A qs as trs i t = loop ctx m 
        loop ctx (Or m1 m2)         = loop ctx m1 ∪ loop ctx m2  
        loop ctx (Ex1 (F k) m)      = A qs as (rmVar1 (F k) trs) i t where 
                A qs as trs i t = loop ctx m 
        loop ctx (Ex2 (S k) m)      = A qs as (rmVar2 (S k) trs) i t where 
                A qs as trs i t = loop ctx m 


rmVar1 (F i) []                    = [] 
rmVar1 (F i) ((q,(a,vs),q'):trs)   = (q,(a, vs'), q') : rmVar1 (F i) trs where 
    vs' = filter ((/=i) . var_num ||$ not . fst_var) vs  

rmVar2 (S i) []                    = [] 
rmVar2 (S i) ((q,(a,vs),q'):trs)   = (q,(a, vs'), q') : rmVar2 (S i) trs where 
    vs' = filter ((/=i) . var_num ||$ fst_var ) vs 



finalizeMSO  (A qs as es is ts) = A qs as' es' is ts where 
    as' = fst <$> as 
    es' = (\(q,a,p) -> if snd a == [] then (q, fst a, p) else error "MSO finalization failure") <$> es 



mso2automata = finalizeMSO . convertMSO 


-- e.g. 

egMSO1      = Ex1 (F 1) (Or (Lab 'a' (Loc 3)) (Lab 'b' (F 1)) )  
egMSO2      = Ex1 (F 1) (Ex1 (F 2) (Suc 0 (F 1) (F 2)))
a1          = mso2automata egMSO1
a2          = mso2automata egMSO2
m1          = matchAutomata a1 "aab"  -- error NFA 
a1'         = renodeAutomata $ trim $ subset_construction a1
m1'         = matchAutomata a1' "caa" 
m1''        = matchAutomata a1' "dba" 
m1'''       = matchAutomata a1' "dddb"
