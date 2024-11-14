module Print where 


import Edge (arrow) 
import Mat 
import Analysis



-- Printing Functions 

printDiag a@(M n _ _ _ _ _) = loop a a n where 
    loop a an n = do 
        print $ show n ++ ": " 
        print $ diag an 
        let a'  = rmLoops an a 
        let an' = mult a' an 
        if n /= 0 then loop a' an' (n-1) else return ()  

printDiag' a@(M n _ _ _ _ _) = loop a a n where 
    loop a an n = do 
        print $ show n ++ ": " 
        print $ (arrow <$>) <$> diag an 
        let a'  = rmLoops an a 
        let an' = mult a' an 
        if n /= 0 then loop a' an' (n-1) else return ()  

printStar a@(M n _ _ _ _ _) = loop a a n where 
    loop a an n = do 
        print $ show n ++ ":\n" 
        print $ convert an 
        print "success path: " 
        print $ success an  
        let a'  = rmLoops an a 
        let an' = mult a an 
        let s   = success an' 
        if n /= 1 then loop a' an' (n-1) else return () 

