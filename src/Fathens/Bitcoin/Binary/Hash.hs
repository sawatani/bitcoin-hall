{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module Fathens.Bitcoin.Binary.Hash (
  Hash160(hash160Data)
, Hash256(hash256Data)
, Hash512(hash512Data)
, hash160
, hash256
, hmac512
) where

import           Control.Monad
import           Crypto.Hash                (Digest, hash)
import           Crypto.Hash.Algorithms     (RIPEMD160, SHA256, SHA512)
import           Crypto.MAC.HMAC            (HMAC, hmac)
import           Data.ByteArray             (ByteArray, ByteArrayAccess, unpack)
import qualified Data.ByteString            as BStrict
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.List
import           Data.Maybe
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as T
import           Data.Text.Lazy.Builder     as TB
import           Data.Word                  (Word8)
import           Fathens.Bitcoin.Binary.Num
import           Numeric                    (readInt, showIntAtBase)

-- Hash Data

data Hash160 = Hash160 { hash160Data :: ByteString } deriving (Eq)
data Hash256 = Hash256 { hash256Data :: ByteString } deriving (Eq)
data Hash512 = Hash512 { hash512Data :: ByteString } deriving (Eq)

-- Hash functions

hash160 :: ByteString -> Hash160
hash160 = Hash160 . (inByteString $ ripemd160 . sha256)

hash256 :: ByteString -> Hash256
hash256 = Hash256 . (inByteString $ sha256 . sha256)

hmac512 :: ByteString -> ByteString -> Hash512
hmac512 key body = (Hash512 . BS.pack . unpack) h
  where
    a = BS.toStrict key
    b = BS.toStrict body
    h = hmac a b :: HMAC SHA512

-- Utilities

ripemd160 :: ByteArrayAccess a => a -> Digest RIPEMD160
ripemd160 = hash

sha256 :: ByteArrayAccess a => a -> Digest SHA256
sha256 = hash

inByteString :: (ByteArrayAccess a) =>
  (BStrict.ByteString -> a) -> ByteString -> ByteString
inByteString f = BS.pack . unpack . f . BS.toStrict
