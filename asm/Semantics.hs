module Semantics where 

import Term 
import Var
import Eval 
import LTS

import Data.HashTable.IO (insert, lookup) 


-- semantics 

s_Stop :: IO State -> IO State 
s_Stop stt = stt 

s_Push :: EXPR -> IO State -> IO State 
s_Push e stt = do 
    (s, m, bind, sto, mem) <- stt
    let i = toInteger (length m) 
    return (X i:s, e:m, bind, sto, mem) 

s_Pop  :: IO State -> IO State 
s_Pop stt = do 
    (x:s, m, bind, sto, mem) <- stt 
    return (s, m, bind, sto, mem) 

s_Swap :: Int -> IO State -> IO State 
s_Swap n stt = do 
    (s, m, bind, sto, mem) <- stt 
    return (loop n s, m, bind, sto, mem) where 
        loop 1 (x:y:s)              = y:x:s
        loop 2 (x:y:z:s)            = z:y:x:s
        loop 3 (x:y:z:a:s)          = a:y:z:x:s 
        loop 4 (x:y:z:a:b:s)        = b:y:z:a:x:s
        loop 5 (x:y:z:a:b:c:s)      = c:y:z:a:b:x:s
        loop 6 (x:y:z:a:b:c:d:s)    = d:y:z:a:b:c:x:s
        loop 7 (x:y:z:a:b:c:d:e:s)  = e:y:z:a:b:c:d:x:s
        loop 8 (x:y:z:a:b:c:d:e:f:s)= f:y:z:a:b:c:d:e:x:s

s_Dup :: Int -> IO State -> IO State 
s_Dup n stt = do 
    (s, m, bind, sto, mem) <- stt
    return (nth n s:s, m, bind, sto, mem) where 
        nth 1 (x:xs) = x
        nth n (x:xs) = nth (n-1) xs
        nth _ _      = error "Invalid Semantics : Cannot DUP" 

s_Assign :: EXPR -> EXPR -> IO State -> IO State 
s_Assign (M i) e stt = do 
    (s, m, bind, sto, mem) <- stt  
    let i' = eval (s, m, bind, sto, mem) i 
    let e' = eval (s, m, bind, sto, mem) e 
    insert mem i' e'
    return (s, m, bind, sto, mem) 
s_Assign (S i) e stt = do 
    (s, m, bind, sto, mem) <- stt  
    let i' = eval (s, m, bind, sto, mem) i
    let e' = eval (s, m, bind, sto, mem) e 
    insert sto i' e'
    return (s, m, bind, sto, mem) 
s_Assign (Var str) e stt = do 
    (s, m, bind, sto, mem) <- stt  
    let i  = toInteger $ length m 
    return (X i:s, e:m, (str,i):bind, sto, mem) where 

s_Cond :: EXPR -> IO State -> IO State
s_Cond b stt = stt

s_Codecopy :: EXPR -> EXPR -> EXPR -> IO State -> IO State
s_Codecopy to from size stt = do 
    (s, m, bind, sto, mem) <- stt
    let sz = eval (s, m, bind, sto, mem) size 
    let f  = eval (s, m, bind, sto, mem) from
    let t  = eval (s, m, bind, sto, mem) to
    mem' <- copy t f sz mem 
    return (s, m, bind, sto, mem) 
        where
            --      n          code[n] 
            code :: Integer -> Integer 
            code = undefined 
            copy :: Integer -> Integer -> Integer -> Mem -> IO Mem 
            copy to fr 0 mem      = return mem 
            copy to fr n mem      = do
                insert mem to (code fr)  
                copy (to+1) (fr+1) (n-1) mem

semantics :: ACTION -> IO State -> IO State 
semantics (Assign x a)              = s_Assign x a
semantics (Stop      )              = s_Stop 
semantics (Dup i     )              = s_Dup i
semantics (Pop       )              = s_Pop 
semantics (Cond b    )              = s_Cond b
semantics (Push e    )              = s_Push e
semantics (Codecopy to frm sz)      = s_Codecopy to frm sz





{--
transition :: Edge -> Configuration -> Configuration
transition (Q i, a, Q j) (Q i', stt) = 
    case i == i' of 
    True    -> (Q j, semantics a stt) 
    False   -> error "edge not defined" 
--}






