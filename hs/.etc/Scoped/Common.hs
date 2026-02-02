    {-# LANGUAGE DataKinds #-} 
    {-# LANGUAGE StandaloneKindSignatures #-} 



module Common where 

import GHC.TypeLits (Symbol) 
import Data.Kind (Type)

data Term       =   Var String 
                |   Lam String Term 
                |   App Term Term 
                deriving (Show) 


type Prefix :: Symbol -> Type 
data Prefix prefix = Prefix 

var :: Prefix "var" 
var = Prefix 

lam :: Prefix "lam" 
lam = Prefix 

app :: Term -> Term -> Term 
app = App
