module Fathens.Bitcoin.Binary.Hex (
  bigEndian
, toBigEndian
, parseAtBase
) where

import           Data.Bits (shiftL, shiftR, (.|.))
import           Data.List (unfoldr)
import           Data.Word (Word8)

bigEndian :: [Word8] -> Integer
bigEndian = foldr f 0 . reverse
  where
    f v i = shiftL i 8 .|. fromIntegral v
toBigEndian :: Integer -> [Word8]
toBigEndian = unfoldr f
  where
    f 0 = Nothing
    f i = Just (fromInteger i :: Word8, shiftR i 8)

parseAtBase :: Int -> (Char -> Maybe Int) -> String -> Maybe Integer
parseAtBase base c2i = foldr f (Just $ toInteger 0) . reverse
  where
    d = toInteger base
    f c v = do
      a <- v
      b <- c2i c
      return $ a * d + (toInteger b)
