module Bind where

import Term  
import Type 



data Bind   = BindName
            | BindTyVAR
            | BindTmVAR Ty 
            | BindTmAbb Term (Maybe Ty) 
            | BindTyAbb Ty 
            deriving (Eq, Show) 


type Ctx = [(ID,Bind)] 


addBind :: Ctx -> ID -> Bind -> Ctx 
addBind ctx x bd    = (x,bd) : ctx   


getBind :: Ctx -> ID -> Bind 
getBind [] i                        = error "getBind: cannot find Bind" 
getBind ((x,bd):ctx) i | i == x     = bd 
                       | otherwise  = getBind ctx i 

bindshift i bd = case bd of 
    BindTyAbb tyT                   -> BindTyAbb (tyShift i tyT) 
    BindTmVAR tyT                   -> BindTmVAR (tyShift i tyT)
    BindTmAbb t Nothing             -> BindTmAbb (tmShift i t) Nothing 
    BindTmAbb t (Just ty)           -> BindTmAbb (tmShift i t) (Just $ tyShift i ty) 

nthBind :: Ctx -> Int -> Bind 
nthBind ctx i | i >= length ctx = error $ "nthBind : " ++ show i ++ " >= len(ctx): " ++ show ctx 
              | otherwise       = bindshift (i+1) bind where 
    bind = snd $ ctx !! i 

getTy ctx i = case nthBind ctx i of 
    BindTmVAR tyT                   -> tyT 
    BindTmAbb _ (Just tyT)          -> tyT 
    BindTmAbb _ (Nothing )          -> error "getTy: No type recorede at the Bind"
    _                               -> error "getTy: Wrong Bind"
