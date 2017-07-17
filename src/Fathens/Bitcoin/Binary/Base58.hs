module Fathens.Bitcoin.Binary.Base58 (
  hexToBase58
, hexFromBase58
) where

import           Data.Bits                  (shiftL, (.|.))
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import           Data.Word                  (Word8)
import           Fathens.Bitcoin.Binary.Hex
import           Numeric                    (showHex, showIntAtBase)

zero :: Word8
zero = BS.head $ C8.singleton '0'

zero' :: Word8
zero' = BS.head chars

chars :: ByteString
chars = C8.pack "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

v2c :: Int -> Char
v2c = C8.index chars

c2v :: Char -> Maybe Int
c2v c = C8.elemIndex c chars

hexToBase58 :: ByteString -> Maybe ByteString
hexToBase58 src = do
  body <- encoded
  return $ (BS.replicate nz zero') `mappend` body
  where
    (z, b) = BS.span (== zero) src
    nz = (BS.length z) `quot` 2
    encoded | BS.null b = Just b
            | otherwise = do
                i <- parseHex b
                return $ C8.pack $ showIntAtBase 58 v2c i ""

hexFromBase58 :: ByteString -> Maybe ByteString
hexFromBase58 base58 = do
  body <- decoded
  n <- Just $ (BS.length body) `mod` 2
  return $ (BS.replicate (nz +n) zero) `mappend` body
  where
    (z, b) = BS.span (== zero') base58
    nz = (BS.length z) * 2
    decoded | BS.null b = Just b
            | otherwise = do
                v <- parseAtBase 58 c2v b
                return $ C8.pack $ showHex v ""
