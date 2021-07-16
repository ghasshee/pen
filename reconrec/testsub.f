/* Examples for testing */

\x:Top. x;
  (\x:Top. x) (\x:Top. x);
 (\ x:Top->Top. x) (\ x:Top. x);
 

(\ r:{x:Top->Top}. r.x r.x) 
  {x=\ z:Top.z, y=\ z:Top.z}; 


"hello";

unit;

\x:A. x;

let x=true in x;

{x=true, y=false}; 
{x=true, y=false}.x;
{true, false}; 
{true, false}.1; 


if true then {x=true,y=false,a=false} else {y=false,x={},b=false};

 2.0 *. 3.14159;

\ x:Bool. x;
(\ x:Bool->Bool. if x false then true else false) 
  (\ x:Bool. if x then false else true); 

\ x:Nat. succ x;
(\ x:Nat. succ (succ x)) (succ 0); 

T = Nat->Nat;
\ f:T. \ x:Nat. f (f x);

