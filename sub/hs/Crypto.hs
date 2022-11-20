module Crypto where 


{-# LANGUAGE FlexibleContexts #-} 

import Crypto.Hash
--import Crypto.Hash.Keccak
import Data.ByteString  (ByteString, unpack ) 
import Data.ByteString.Lazy (toStrict, fromStrict) 
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.ByteArray   (convert) 
import Text.Printf (printf)
import Data.Char

import Hex (fromhexChar) 

sha3            ::  ByteString -> ByteString
sha3    bs      =   convert (hash bs :: Digest Keccak_256)

from    :: String -> ByteString
from    = toStrict . fromString

fromHex   = toStrict . fromString . map (chr . fromInteger . fromhexChar) 

toHex     :: ByteString -> String
toHex     = map toUpper . concatMap (printf "%02x") . unpack



