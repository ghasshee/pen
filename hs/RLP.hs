{-# LANGUAGE OverloadedStrings #-} 

import Data.ByteString hiding (map, concatMap) 
-- import qualified Data.ByteString.Char8 as C

import Data.Word (Word8)
import Numeric (showHex) 

import Prelude hiding (length, concat, head, reverse) 


data RLP    = RLPString ByteString
            | RLPList [RLP] 
            deriving (Show, Eq) 

{--
encodeLength :: Int -> Word8 -> ByteString 
encodeLength len offset 
    | len <= 55     = singleton (fromIntegral len + offset)
    | otherwise     = cons a lenBytes where 
        a = fromIntegral(length lenBytes) + offset + 55
        lenBytes = int2Bytes len 
--}

int2Bytes :: Int -> ByteString 
int2Bytes 0 = singleton 0 
int2Bytes n = reverse (unfoldr step n) where 
    step 0 = Nothing 
    step i = Just (fromIntegral (i `mod` 256), i `div` 256) 


rlpEncode :: RLP -> ByteString 
rlpEncode (RLPString bs) 
    | length bs == 1 && head bs < 0x80 = bs 
    | length bs <= 55 = cons (0x80 + fromIntegral (length bs)) bs
    | otherwise         = concat [singleton (0xb7 + fromIntegral (length lenBytes)), lenBytes, bs] where 
        lenBytes = int2Bytes (length bs)
rlpEncode (RLPList xs) =
    let payload = concat (map rlpEncode xs)
        len     = length payload in 
    if len <= 55 
        then    cons (0xc0 + fromIntegral len) payload
        else    let lenBytes = int2Bytes len in 
                concat [singleton (0xf7 + fromIntegral (length lenBytes)), lenBytes, payload] 




--- e.g. 
r1  = (RLPList [RLPString "cat"]) 
r1' = (RLPString "cat") 

e1  = rlpEncode r1
e1' = rlpEncode r1' 


showRLP r = "0x" ++ concatMap (`showHex` "") (unpack (rlpEncode r)) 
