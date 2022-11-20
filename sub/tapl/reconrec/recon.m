/* let k = \x.\y.x in

let s = \x.\y.\z.x z(y z) in 
s k(s s (k k (s k k k )) k) k;
*/ 

 (\x.x) (\x.\y.x) ;

let id = \x. x in 
let a = id true in 
id 0;
/* (\f. \x. let g = f in g 0) (\x:Bool. x) true ; */
(\f. \x. let g = f in g x) (\x:Bool. if x then 1 else 0 ) true ;

letrec fac : Nat -> Nat = \x. if iszero x then 1 else x * (fac (pred x)) in fac 6;
 
