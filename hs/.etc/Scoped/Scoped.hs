--{-# OPTIONS_GHC -fno-warn-orphans #-} 

{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE MagicHash #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE OverloadedRecordDot #-} 
-- {-# LANGUAGE GADTs #-} 
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE TypeApplications #-} 
--{-# LANGUAGE TypeOperators #-} 
--{-# LANGUAGE PolyKinds #-} 
--{-# LANGUAGE DataKinds #-} 
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE FlexibleInstances #-} 
--{-# LANGUAGE MultiParamTypeClasses #-} 
--{-# LANGUAGE ConstraintKinds #-} 
--{-# LANGUAGE StandaloneKindSignatures #-} 


module Scoped where 

import Common

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal', ErrorMessage(Text), TypeError, ErrorMessage((:<>:)))
import GHC.Records (HasField, getField) 
import GHC.Exts
import Unsafe.Coerce


type IsScoped :: Symbol -> () 
type family IsScoped name 

type InScope name = IsScoped name ~ '() 

data Enter name where 
    Enter :: InScope name => Enter name 


instance (res ~ ((Enter name -> Term) -> Term), KnownSymbol name) => 
    HasField name (Prefix "lam") res where
        getField _ k = 
            case unsafeEqualityProof @(IsScoped name) @'() of 
              UnsafeRefl -> Lam (symbolVal' (proxy# @name)) $ k Enter

type FreeVariableError :: Symbol -> Constraint
type family FreeVariableError name where 
    FreeVariableError name = 
        TypeError ('Text "Can't reference a free variable '" :<>: 'Text name :<>: 'Text "'") 


type ThrowOnFree :: Constraint -> () -> Constraint
type family ThrowOnFree err scoped where 
    ThrowOnFree _ '() = ()
    ThrowOnFree err _ = err

instance (res ~ Term, KnownSymbol name, ThrowOnFree (FreeVariableError name) (IsScoped name)) => 
    HasField name (Prefix "var") res where 
        getField _ = Var $ symbolVal' (proxy# @name)

owl :: Term 
owl = lam.f $ \Enter -> lam.g $ \Enter -> app var.g (app var.f var.g) 

i :: Term
i = lam.x $ \Enter -> var.x

i' = Lam "x" (Var "x") 
f = Var "x" 

--free :: Term
--free = var.x 



