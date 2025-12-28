module LTL2Buchi where 

import Data.Set (Set) 
import Set 
import Edge 
import AST
import Term 


type Node = Int 
type Incoming a = [Edge Int a ]  
type TobeDone = Set Formulae 
type Old = Set Formulae 
type Next = Set Formulae 
type Eventualities = Set Formulae 
type Accepting = [Int] 
type EquivClass = Int 
