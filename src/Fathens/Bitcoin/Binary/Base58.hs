module Fathens.Bitcoin.Binary.Base58 (
  encodeBase58
, decodeBase58
) where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import           Data.List                  (elemIndex, (!!))
import           Data.Maybe                 (fromMaybe, isJust, listToMaybe)
import           Data.Tuple                 (fst, snd)
import           Data.Word                  (Word8)
import           Fathens.Bitcoin.Binary.Num
import           Numeric                    (readInt, showIntAtBase)

zero :: Word8
zero = BS.head $ C8.singleton $ head chars

chars :: [Char]
chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

v2c :: Int -> Char
v2c = (chars !!)

c2v :: Char -> Maybe Int
c2v c = elemIndex c chars

encodeBase58 :: [Word8] -> ByteString
encodeBase58 src = (BS.replicate nz zero) `mappend` encoded
  where
    (z, b) = span (== 0) src
    nz = length z
    encoded | null b = BS.empty
            | otherwise = C8.pack $ showIntAtBase 58 v2c (fromBigEndian b) ""

decodeBase58 :: ByteString -> Maybe [Word8]
decodeBase58 base58 = do
  body <- decoded
  return $ (replicate nz 0) `mappend` body
  where
    (z, b) = BS.span (== zero) base58
    nz = BS.length z
    decoded | BS.null b = Just []
            | otherwise = do
                let p = isJust . c2v
                let f = fromMaybe (error "Invalid Base58 char") . c2v
                (v,o) <- listToMaybe $ readInt 58 p f $ C8.unpack b
                r <- case o of
                  [] -> Just v
                  _  -> Nothing
                return $ toBigEndian r
