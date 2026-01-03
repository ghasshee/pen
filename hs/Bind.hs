module Bind where

import Term  
import Type 



data Bind   = BindName
            | BindTyVAR
            | BindTmVAR Ty 
            | BindTmABB Term (Maybe Ty) 
            | BindTyABB Ty 
            deriving (Eq, Show) 


type Ctx = [(ID,Bind)] 


addBind :: Ctx -> ID -> Bind -> Ctx 
addBind ctx x bd    = (x,bd) : ctx   


getBind :: Ctx -> ID -> Bind 
getBind [] i                        = error "getBind: cannot find Bind" 
getBind ((x,bd):ctx) i | i == x     = bd 
                       | otherwise  = getBind ctx i 

bindshift i bd = case bd of 
    BindTyABB tyT                   -> BindTyABB (tyShift i tyT) 
    BindTmVAR tyT                   -> BindTmVAR (tyShift i tyT)
    BindTmABB t Nothing             -> BindTmABB (tmShift i t) Nothing 
    BindTmABB t (Just ty)           -> BindTmABB (tmShift i t) (Just $ tyShift i ty) 

nthBind :: Ctx -> Int -> Bind 
nthBind ctx i | i >= length ctx = error $ "nthBind : " ++ show i ++ " >= len(ctx): " ++ show ctx 
              | otherwise       = bindshift (i+1) bind where 
    bind = snd $ ctx !! i 

getTy ctx i = case nthBind ctx i of 
    BindTmVAR tyT                   -> tyT 
    BindTmABB _ (Just tyT)          -> tyT 
    BindTmABB _ (Nothing )          -> error "getTy: No type binded"
    _                               -> error "getTy: Wrong Bind"
