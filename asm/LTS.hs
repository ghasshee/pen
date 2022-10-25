{-# LANGUAGE GADTs #-} 

module LTS where

-- Labelled Transition System 

import Data.HashTable.IO (BasicHashTable, new, insert, lookup)

import Opcode 
import Var
import Term
import Tree


type Stack          = [Var] 
type Mapping        = [EXPR] 
type Bind           = [(String, Integer)]
type Sto            = BasicHashTable Integer Integer
type Mem            = BasicHashTable Integer Integer

type State          = (Stack, Mapping, Bind, Sto, Mem) 


data Node           = Q Int deriving (Show, Eq, Read)
type Edge           = (Node, ACTION, Node) 
type Configuration  = (Node, State)  



-- after Knit function 
-- we should label with Node Number 

mkNode :: [RBTree OPCODE] -> [(Node, RBTree OPCODE)] 
mkNode ts = loop 0 ts where
    loop n [] = [] 
    loop n (t:ts) = (Q n,t) : loop (n+1) ts



