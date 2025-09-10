{-# LANGUAGE OverloadedStrings #-} 


module EVM where 



import GCLL

import Data.Map.Strict hiding (drop) 
import Data.Word

import Data.ByteString (ByteString, concat, unfoldr, drop)

import Prelude hiding (drop, concat ) 


import RLP 
import Crypto

-- Exec Env I 

data I  = I_a   -- Account
        | I_o   -- Originator
        | I_p   -- Price of gas 
        | I_d   -- Input Data
        | I_s   -- Sender
        | I_v   -- Value
        | I_b   -- Byte Code of Initialization
        | I_e   -- dEpth of Call/Create
        | I_w   -- The permission to make modification to state
             





data EVM = EVM  { stk  :: [EXPR] 
                , mem  :: Map EXPR EXPR  
                , stor :: Map EXPR EXPR
                , code :: [Word8] 
                } deriving (Show) 


emptyEVM = EVM [] empty empty 








---------------------------------------
---  CREATE / CREATE2 Addr          ---
---------------------------------------

-- int 2 big endian bytes
int2BE :: Integer -> ByteString
int2BE 0 = ""
int2BE n 
    | n > 0     = rev (unfoldr step n) 
    | otherwise = error "int2BE: negative number" where 
        step 0 = Nothing
        step i = Just (fi (i `mod` 256), i `div` 256) 


computeCREATEAddr :: ByteString -> Integer -> ByteString 
computeCREATEAddr addr nonce 
    | len addr /= 20    = error "computeCREATEAddr: bad addr" 
    | otherwise         = drop 12 h     where 
    rlp = encode $ RLPList [RLPString addr, RLPString nonce']
    h   = sha3 rlp  
    nonce' = int2BE nonce 



computeCREATE2Addr :: ByteString -> ByteString -> ByteString -> ByteString 
computeCREATE2Addr addr salt init_code
    | la /= 20 || ls /= 32  = error "computeCREATE2Addr: bad addr" 
    | otherwise             = sha3 $ concat ["\255", addr, salt, code]
    where 
        la = len addr
        ls = len salt 
        code = sha3 init_code


