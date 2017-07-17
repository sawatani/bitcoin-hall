module Fathens.Bitcoin.Binary.Hex (
  parseHex
, parseAtBase
) where

import           Data.Bits             (shiftL, (.|.))
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (elemIndex, pack, unpack)
import           Data.Char             (toLower)

hexs :: ByteString
hexs = pack "0123456789abcdef"

hexChar2Int :: Char -> Maybe Int
hexChar2Int c = elemIndex (toLower c) hexs

parseHex :: ByteString -> Maybe Integer
parseHex = parseAtBase 16 hexChar2Int

parseAtBase :: Int -> (Char -> Maybe Int) -> ByteString -> Maybe Integer
parseAtBase base c2i = foldr f (Just $ toInteger 0) . reverse . unpack
  where
    d = toInteger base
    f c v = do
      a <- v
      b <- c2i c
      return $ a * d + (toInteger b)
