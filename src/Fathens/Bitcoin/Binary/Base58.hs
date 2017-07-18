module Fathens.Bitcoin.Binary.Base58 (
  encodeBase58
, decodeBase58
) where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import           Data.Word                  (Word8)
import           Fathens.Bitcoin.Binary.Hex
import           Numeric                    (showIntAtBase)

zero :: Word8
zero = BS.head chars

chars :: ByteString
chars = C8.pack "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

v2c :: Int -> Char
v2c = C8.index chars

c2v :: Char -> Maybe Int
c2v c = C8.elemIndex c chars

encodeBase58 :: [Word8] -> ByteString
encodeBase58 src = (BS.replicate nz zero) `mappend` encoded
  where
    (z, b) = span (== 0) src
    nz = length z
    encoded | null b = BS.empty
            | otherwise = C8.pack $ showIntAtBase 58 v2c (bigEndian b) ""

decodeBase58 :: ByteString -> Maybe [Word8]
decodeBase58 base58 = do
  body <- decoded
  let n = (length body) `mod` 2
  return $ (replicate (nz +n) 0) `mappend` body
  where
    (z, b) = BS.span (== zero) base58
    nz = BS.length z
    decoded | BS.null b = Just []
            | otherwise = do
                v <- parseAtBase 58 c2v $ C8.unpack b
                return $ toBigEndian v
