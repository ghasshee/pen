module Semantics where 


import Tree
import Asm 


type Value = Integer
type Address = Integer 

data Mem = M Address Value 

semantics :: RBTree OPCODE -> Mem -> Mem 
semantics (RED MSTORE ts) =  undefined 
