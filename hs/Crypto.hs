{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE PackageImports #-} 

module Crypto where 


import "crypton" Crypto.Hash
import Data.ByteString  (ByteString, unpack ) 
import Data.ByteString.Lazy (toStrict, fromStrict) 
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.ByteArray   (convert) 
import Text.Printf (printf)

import Data.Char (toUpper) 

sha3            ::  ByteString -> ByteString
sha3    bs      =   convert (hash bs :: Digest Keccak_256)

from    :: String -> ByteString
from    = toStrict . fromString


toHex     :: ByteString -> String
toHex     = map toUpper . concatMap (printf "%02x") . unpack





dispatcherHash :: String -> String 
dispatcherHash = Prelude.take 8 . toHex . sha3 . from 

-- e.g. 
-- function set (uint: val) public  { 
a = dispatcherHash "set(uint256)" 
b = dispatcherHash "inc()"
