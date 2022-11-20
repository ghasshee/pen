Counter         = { get: Unit -> Nat, inc : Unit -> Unit } ;
ResetCounter    = { get: Unit -> Nat, inc : Unit -> Unit, reset: Unit -> Unit };
CounterRep      = { x: Ref Nat};

newCounter          = \a:Unit. let x = ref 1 in 
                        {   get     = \a:Unit. !x,
                            inc     = \a:Unit. x:=succ(!x) };

a = newCounter ();
a.inc ();
a.inc ();
a.get ();


counterClass        =   \r:CounterRep. 
                        {   get     = \u:Unit. !(r.x), 
                            inc     = \u:Unit. r.x := succ(!(r.x)) } ;

newCounter          = \u:Unit. let r = { x = ref 0 } in counterClass r;

c                   = newCounter ();
c.inc();c.inc();
c.get();

resetCounterClass   =   \r:CounterRep.
                        { get   = \u:Unit. !(r.x),
                          inc   = \u:Unit. r.x := succ(!(r.x)),
                          reset = \u:Unit. r.x := 0 } ; 

newResetCounter     =   \u:Unit. let r = {x = ref 0 } in resetCounterClass r;

c                   =   newResetCounter (); 
c.inc(); c.inc(); c.get(); c.reset(); c.get();


BackupCounter       =   { get : Unit -> Nat, inc : Unit -> Unit, reset : Unit -> Unit, backup : Unit -> Unit };

BackupCounterRep    =   { x : Ref Nat, b : Ref Nat };

backupCounterClass  =   \r:BackupCounterRep.
                        let super = resetCounterClass r in 
                        {   get     = super.get,
                            inc     = super.inc,
                            reset   = \u:Unit. r.x := !(r.b),
                            backup  = \u:Unit. r.b := !(r.x)    };

newBackupCounter    =   \u:Unit. let r = { x = ref 0, b = ref 0 } in backupCounterClass r ;

c = newBackupCounter () ; 
c.inc () ; c.backup(); c.inc () ; c.inc() ; c.get (); c.reset (); c.get () ;



/* SetCounter */

SetCounter          = { get: Unit->Nat, set: Nat->Unit, inc: Unit->Unit };
setCounterClass     = \r:CounterRep. fix (\self: SetCounter.
                        {   get = \u:Unit. !(r.x),
                            set = \n:Nat. r.x := n,
                            inc = \u:Unit. self.set (succ (self.get unit))      } );

newSetCounter       = \u:Unit. let r = { x = ref 1 } in setCounterClass r ;
c = newSetCounter ();

c.set 10; 
c.inc (); 
c.get (); 


/* SetCounter 2 */ 
setCounterClass     = \r:CounterRep. \self:SetCounter. 
                        {   get = \u:Unit. !(r.x),
                            set = \n:Nat. r.x := n,
                            inc = \u:Unit. self.set (succ (self.get ()))    } ;
newSetCounter       = \u:Unit. let r = { x = ref 0 } in fix (setCounterClass r);

c = newSetCounter ();
c.set 10;
c.inc ();
c.get ();


InstrCounter        = { get : Unit -> Nat, set : Nat -> Unit, inc : Unit->Unit, accesses : Unit -> Nat } ;
InstrCounterRep     = { x : Ref Nat, a: Ref Nat } ;
instrCounterClass   = \r:InstrCounterRep. \self:InstrCounter. 
                        let super = setCounterClass r self in 
                        {   get = super.get,
                            set = \n:Nat. (r.a := succ(!(r.a)); super.set n),
                            inc = super.inc,
                            accesses = \u:Unit. !(r.a) } ;

newInstrCounter     = \u:Unit. let r = { x = ref 0, a = ref 0 } in fix ( instrCounterClass r );
/*
c = newInstrCounter () ;
c.set 10;
c.inc (); 
c.inc 10; 
c.accesses ();
*/

/* SetCounter 3 - InstrCounter Can Work with Instances  */ 
/*
setCounterClass     = \r:CounterRep. \self:Unit -> SetCounter. \u:Unit. 
                        {   get = \u:Unit. !(r.x),
                            set = \n:Nat. r.x := n,
                            inc = \u:Unit. (self ()).set (succ ((self ()).get ()))    } ;
newSetCounter       = \u:Unit. let r = { x = ref 0 } in fix (setCounterClass r) () ;

c = newSetCounter ();
c.set 10;
c.inc ();
c.get ();

InstrCounter        = { get : Unit -> Nat, set : Nat -> Unit, inc : Unit->Unit, accesses : Unit -> Nat } ;
InstrCounterRep     = { x : Ref Nat, a: Ref Nat } ;
instrCounterClass   = \r:InstrCounterRep. \self:Unit -> InstrCounter. \u:Unit.
                        let super = setCounterClass r self () in 
                        {   get = super.get,
                            set = \n:Nat. (r.a := succ(!(r.a)); super.set n),
                            inc = super.inc,
                            accesses = \u:Unit. !(r.a) } ;

newInstrCounter     = \u:Unit. let r = { x = ref 0, a = ref 0 } in fix ( instrCounterClass r ) ();
c = newInstrCounter () ;
c.set 10;
c.inc (); 
c.inc ();
c.accesses ();
*/


/* SetCounter 4 - Efficient SetCounter */

setCounterClass     = \r:CounterRep. \self: Source SetCounter.
                        {   get = \u:Unit. !(r.x),
                            set = \n:Nat. r.x := n,
                            inc = \u:Unit. (! self).set (succ ((! self).get ()))  } ;
dummySetCounter     = { get = \u:Unit. 0, set = \n:Nat. (), inc = \u:Unit. () } ;
newSetCounter       = \u:Unit.
                        let r = { x = ref 1 } in
                        let cAux = ref dummySetCounter in 
                        (cAux := (setCounterClass r cAux); !cAux);

InstrCounter        = { get : Unit -> Nat, set : Nat -> Unit, inc : Unit->Unit, accesses : Unit -> Nat } ;
InstrCounterRep     = { x : Ref Nat, a: Ref Nat } ;
instrCounterClass   = \r:InstrCounterRep. \self: Source InstrCounter.
                        let super = setCounterClass r self in 
                        {   get = super.get,
                            set = (\n:Nat. ((r.a := succ(!(r.a))) ; super.set n)),
                            inc = super.inc,
                            accesses = \u:Unit. !(r.a)  } ; 

dummyInstrCounter   = { get = \u:Unit. 0, set = \n:Nat. (), inc = \u:Unit.(), accesses = \u:Unit.0 } ; 
newInstrCounter     = \u:Unit.
                        let r = { x = ref 1, a = ref 0 } in
                        let cAux = ref dummyInstrCounter in 
                        (cAux := (instrCounterClass r cAux); !cAux);

c                   = newInstrCounter ();
c.inc ();

(((\x:Nat.(x))) 1);
