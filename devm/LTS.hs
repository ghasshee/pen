{-# LANGUAGE GADTs #-} 
{-# LANGUAGE FlexibleInstances #-} 

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




