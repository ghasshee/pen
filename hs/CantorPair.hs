module CantorPair where 



import Utils 


------------------------------------
--- Encode with  cantor Pairing  ---
------------------------------------

i_sqrt 0 = 0 
i_sqrt 1 = 1 
i_sqrt n = hd $ dropWhile (not . isRoot) iters where 
    (_R, _N) = last $ takeWhile ((n>=) . snd) $ zip (1:pow2s) pow2s 
    newton x = (x + n `div` x) `div` 2
    iters = iterate newton (i_sqrt (n `div` _N) * _R)
    isRoot r = r ^ 2 <= n && n < (r+1) ^ 2 
    pow2s  = iterate (^2) 2   


cantorPair :: (Integer, Integer) -> Integer 
cantorPair (x,y) = (x + y) * (x + y + 1) `div` 2 + y  

cantorUnpair :: Integer -> (Integer,Integer) 
cantorUnpair z = (w-y, y) where 
    y = z - t
    t = w * (w+1) `div` 2
    w = (i_sqrt (8 * z + 1) - 1) `div` 2



data DT     = DNIL
            | DCONST  
            | DPAIR DT DT 
            
            deriving (Show, Eq, Read) 

encodeDT  :: DT -> Integer 
encodeDT  DNIL                  = 0 
encodeDT  DCONST                = 1     
encodeDT  (DPAIR d e)         = cantorPair (encodeDT d, encodeDT e)


decodeDT  :: Integer -> DT 
decodeDT 0 = DNIL
decodeDT 1 = DCONST
decodeDT n = 
    let (i,j) = cantorUnpair n in 
    DPAIR (decodeDT i)(decodeDT j) 

        
depth :: DT -> Integer
depth DNIL = 0
depth DCONST = 1
depth (DPAIR d e) = 1 + max (depth d) (depth e) 


leng :: DT -> Integer 
leng DNIL = 0 
leng DCONST = 1
leng (DPAIR d e) = leng d + leng e 


balanceBinTree = undefined 

