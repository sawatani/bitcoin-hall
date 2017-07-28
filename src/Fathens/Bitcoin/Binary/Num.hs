module Fathens.Bitcoin.Binary.Num (
  fromBigEndian
, toBigEndian
) where

import           Data.Bits            (shiftL, shiftR, (.|.))
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

fromBigEndian :: ByteString -> Integer
fromBigEndian = BS.foldr f 0 . BS.reverse
  where
    f v i = shiftL i 8 .|. fromIntegral v

toBigEndian :: Integer -> ByteString
toBigEndian = BS.reverse . BS.unfoldr f
  where
    f 0 = Nothing
    f i = Just (fromInteger i, shiftR i 8)
