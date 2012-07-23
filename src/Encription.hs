module Encription 
	( encrypt
	, decrypt
	) where

import Codec.Crypto.AES
import Codec.Crypto.AES.Random
import Data.Aeson

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

import Types

encrypt :: String -> FilePath -> Safe -> IO (C.ByteString, C.ByteString)
encrypt key path m = do
    iv <- randBytes 16
    let cm = crypt' CFB (C.pack key) iv Encrypt (toStrict $ encode m)
    return $ (iv, cm)
  where
    toStrict :: L.ByteString -> C.ByteString
    toStrict = C.concat . L.toChunks

decrypt :: String -> FilePath -> IO (C.ByteString)
decrypt key path = do
    d <- C.readFile path
    let (iv, c) = C.splitAt 16 d
    return $ crypt' CFB (C.pack key) iv Decrypt c