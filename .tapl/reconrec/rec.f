Counter = Rec P. {get:Nat, inc:Unit->P } ;
p   = 
    let create = fix (\cr:{x:Nat} -> Counter. \s:{x:Nat}.fold [Counter]{get = s.x, inc = \u:Unit. cr{x=succ(s.x)}}) in 
    create {x=0};

p1 = (unfold [Counter] p).inc unit;
(unfold [Counter] p1).get;



Hungry = Rec A. Nat -> A ;
f = fix (\f:Nat->Hungry. \n:Nat. fold [Hungry] f) ;
u = unfold [Hungry];
u( u( u (f 0) 1) 2) 3;
