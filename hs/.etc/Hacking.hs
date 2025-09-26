
import Control.Exception
import Data.IORef 
import System.IO.Unsafe 

lastIsFalse :: [Bool] 
lastIsFalse = unsafePerformIO $ do
    next <- newIORef False 
    let go = do 
            b   <- unsafeInterleaveIO $ readIORef next 
            bs  <- unsafeInterleaveIO $ do 
                writeIORef next True
                _ <- evaluate b
                writeIORef next False 
                go 
            return (b : bs) 
    go 




