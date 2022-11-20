
true;
if false then true else false;

(\x:Bool->Bool.x false) (\x:Bool.x) ;
(\x:Bool.x) true;
iszero (pred (succ 2));

let projx = (\r:{x:Nat,y:Nat,z:Nat}.r.x) in projx {x=2,y=4,z=1} ;
(\x:String.x) "hoge";

case <some=1> as <some:Nat,none:Unit> of <some=a> => succ a | <none=y> => 0; 
let g = (\x:Nat.succ x) in g 1;
let f = (\x : <some:Nat,none:Unit>. case x of <some=a> => succ a | <none=y> => 0) in 
f (<some=1> as <some:Nat,none:Unit>) ; 

X = Nat; 
(\x:X.succ x) 1;
Y = X -> X; 
s = (\x:X.succ x);
double = \f:Y.\x:X.f(f x);
double s 1;


ff = \ie:Nat->Bool.
        \x:Nat.
        if iszero x then true
        else if iszero (pred x) then false 
        else ie (pred (pred x));

iseven = fix ff;



iseven 7;


a = ref 0;
incr = ( \a:(Ref Nat). a := succ( !a ) );

!a ;
incr a ;
!a ; 
