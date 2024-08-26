module Any where 

import Set 



data Any a = Any [a] [a] 
    -- the first list is the list of included elements  --  {x,y,z}
    -- the second list is the list of excluded elements --   a \ { x,y,z }  
    deriving (Read) 


evalAny (Any is []) = Any is []
evalAny (Any [] es) = Any [] es
evalAny (Any is es) = if cs == [] 
                        then Any is es
                        else Any (setminus is cs) (setminus es cs) where
                            cs = setintersect is es
                    

instance Show a => Show (Any a) where 
    show (Any is []) = show is 
    show (Any [] es) = "{..}\\{" ++ show es ++ "}"   









