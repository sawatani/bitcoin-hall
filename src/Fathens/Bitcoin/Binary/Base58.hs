{-# LANGUAGE MultiParamTypeClasses #-}
module Fathens.Bitcoin.Binary.Base58 (
  Base58
, base58
, base58Text
, encodeBase58
, decodeBase58
) where

import           Control.Monad
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

zero :: Char
zero = head chars

chars :: [Char]
chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

data Base58 = Base58 Text deriving (Show, Eq)

base58 :: Text -> Maybe Base58
base58 ts = Base58 ts <$ guard isValid
  where
    isValid = flip elem chars `T.all` ts

base58Text :: Base58 -> Text
base58Text (Base58 t) = t

encodeBase58 :: ByteString -> Base58
encodeBase58 src = Base58 $ T.pack $ (zeros . encoded) ""
  where
    (z, b) = BS.span (== 0) src
    zeros = showString $ flip replicate zero $ fromIntegral $ BS.length z
    encoded | BS.null b = id
            | otherwise = showIntAtBase 58 (chars !!) $ fromBigEndian b

decodeBase58 :: Base58 -> ByteString
decodeBase58 (Base58 src) = zeros `mappend` decoded
  where
    (z, b) = T.span (== zero) src
    zeros = T.length z `BS.replicate` 0
    decoded = toBigEndian $ readBase58 $ T.unpack $ T.reverse b

readBase58 :: [Char] -> Integer
readBase58 [] = 0
readBase58 (h:o) = c2v h + 58 * readBase58 o
  where
    c2v c = toInteger $ fromJust $ elemIndex c chars
