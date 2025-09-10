

open Misc

type semiring   = rig 

and  nat        = O 
                | S of nat

and  rig        = N of nat
                | Infty

let rec rig_of_int  = 
    let rec nat_of_int  = function 
        | 0                 -> O 
        | n                 -> S(nat_of_int(n-1)) in function
    |  n when n>=0      -> N (nat_of_int n)
    |  -1               -> Infty 
    |  _                -> err"rig_of_int"  

let rec int_of_rig  = function 
    | N(O)              -> 0 
    | N(S p)            -> 1 + int_of_rig (N p) 
    | Infty             -> -1 

let rec str_of_rig n= match int_of_rig n with 
    | -1                -> "∞"
    | n                 -> str_of_int n 

let rec add n       = function 
    | N(O)              -> n 
    | N(S(p))           -> let N(n) = n in add (N(S n)) (N(p))
    | Infty             -> Infty 

let rec mul n       = function 
    | N(O) when n=Infty -> pe ("illegal multiplication: ∞ * 0"); err "" 
    | N(O)              -> N(O)
    | N(S(p))           -> add n (mul n (N(p)))
    | Infty             -> Infty

