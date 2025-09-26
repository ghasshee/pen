module Arity where 


--  Arity defines the arity of functions 
--      that is,  higher order / first order .

--  Arity 0 of an argument means that 
--      the argument is 1st order i.e. the argument does not have any arguments 
--      In other words, 
--      the type of arity 0 arg is NOT ARROW TYPE. 
--  Arity 1 means 
--      its type is Arrow type of the form (_ -> _) 
--  Arity 2 means 
--      its type is ((_ -> _) -> _) 
--      that is, the argment takes 1st order function as its argument. 
--  Arity n is defined inductively. 





data Arity = Arity Int 
