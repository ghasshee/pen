{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE PackageImports #-} 
{-# LANGUAGE IncoherentInstances #-} 

module RLP where 
import Data.ByteString hiding (map, concatMap )

import Data.Word (Word8)

import Prelude hiding (length, concat, head, tail, reverse, null, take, drop)


len     = length 
hd      = head
tl      = tail 
rev     = reverse
fi a    = fromIntegral a 


type Parser a = Either String (a,ByteString) 

data RLP    = RLPString ByteString
            | RLPList [RLP] 
            deriving (Show, Eq) 


bytes :: Int -> ByteString 
bytes 0 = singleton 0 
bytes n = rev (unfoldr step n) where 
    step 0 = Nothing 
    step i = Just (fi (i `mod` 256), i `div` 256) 


encodeRLP :: RLP -> ByteString 
encodeRLP (RLPString "") = ""
encodeRLP (RLPString bs)    
    | l == 1 && hd<0x80 = bs 
    | l <= 55           = cons (0x80 + fi l ) bs
    | otherwise         = concat [singleton (0xb7 + fi (len h)), h, bs] where 
        hd  = head bs 
        h    = bytes l
        l   = len bs 
encodeRLP (RLPList xs) 
    | l <= 55           = cons (0xc0 + fi l) bs
    | otherwise         = concat [singleton (0xf7 + fi (len h)), h, bs] where 
        h   = bytes l 
        bs  = concat (map encodeRLP xs)
        l   = len bs 



decodeRLP :: ByteString -> Parser RLP 
decodeRLP bs 
    | null bs       = Left "decodeRLP: empty input" 
    | b < 0x80      = Right (RLPString (singleton b), bs')   
    | b < 0xc0      = Right (RLPString (take l bs'), drop l bs')    
    | b < 0xf8      = do    (rlps, r)       <- decodeList bss 
                            Right (RLPList rlps, r)
    | otherwise     = do    (rlps, r)       <- decodeList bss'     
                            Right (RLPList rlps, r)
    where 
        l   = fi b - 0x80 
        l'  = fi b - 0xc0
        b   = hd bs 
        bs' = tl bs 
        bss = take l' bs' 
        l'' = fi b - 0xf7 
        bss' = take l'' bs' 

decodeList :: ByteString -> Parser [RLP] 
decodeList bss   
    | null bss      = Right ([], "")
    | otherwise     = do 
        (x, rest)   <- decodeRLP bss 
        (xs, r)     <- decodeList rest
        Right (x:xs, r) 




encode :: RLP -> ByteString 
encode = encodeRLP

decode :: ByteString -> RLP 
decode bs = case decodeRLP bs of 
    Left err        -> error err
    Right (rlp, _)  -> rlp



--- e.g. 
r1  = (RLPList [RLPString "cat"]) 
r1' = (RLPString "cat") 
r0  = RLPString ""

e1  = encode r1
e1' = encode r1' 
e0  = encode r0 



