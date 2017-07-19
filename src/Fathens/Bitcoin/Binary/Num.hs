module Fathens.Bitcoin.Binary.Num (
  fromBigEndian
, toBigEndian
) where

import           Data.Bits (shiftL, shiftR, (.|.))
import           Data.List (unfoldr)
import           Data.Word (Word8)

fromBigEndian :: [Word8] -> Integer
fromBigEndian = foldr f 0 . reverse
  where
    f v i = shiftL i 8 .|. fromIntegral v
toBigEndian :: Integer -> [Word8]
toBigEndian = reverse . unfoldr f
  where
    f 0 = Nothing
    f i = Just (fromInteger i :: Word8, shiftR i 8)
