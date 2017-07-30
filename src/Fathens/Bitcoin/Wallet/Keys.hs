module Fathens.Bitcoin.Wallet.Keys where

import           Control.Monad
import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy           as BS
import qualified Data.ByteString.Lazy.Char8     as C8
import           Data.List
import           Data.Maybe
import           Data.Text.Lazy                 (Text)
import qualified Data.Text.Lazy                 as T
import           Data.Word                      (Word8)
import           Fathens.Bitcoin.Binary.Base58
import           Fathens.Bitcoin.Binary.Num
import qualified Fathens.Bitcoin.Wallet.Address as AD

-- Data

data PublicKey = PublicKey ECPoint deriving (Show, Eq)
data ECPoint =
  UncompressedPoint {
    ecPointX :: Integer
  , ecPointY :: Integer
  } |
  CompressedPoint {
    ecValueX :: Integer
  , ecFlagY  :: Word8
  }
  deriving (Show, Eq)

data PrivateKey = PrivateKey AD.AddressPrefix Integer deriving (Show, Eq)

-- Classes

-- Functions

readPrvKey :: Base58 -> Maybe PrivateKey
readPrvKey b58 = do
  prefix <- findPrefixPrv b58
  d <- decodeBase58Check b58
  let payload = AD.getPayload prefix d
  let k = fromBigEndian payload
  return $ PrivateKey prefix k

prvKeyWIF :: PrivateKey -> Base58
prvKeyWIF (PrivateKey prefix k) = encodeBase58Check d
  where
    d = AD.appendPayload prefix $ toBigEndian k

pubKeyAddress :: PublicKey -> Base58
pubKeyAddress (PublicKey (UncompressedPoint x y)) = encodeBase58Check body
  where
    body = 0x04 `BS.cons` toBigEndian x `BS.append` toBigEndian y

-- Utilities

findPrefixPrv :: Base58 -> Maybe AD.AddressPrefix
findPrefixPrv = (AD.findBySymbol [AD.prefixPRV, AD.prefixCPRV]) . base58Text

decompressPoint :: ECPoint -> ECPoint
decompressPoint (CompressedPoint x z) = UncompressedPoint x x
decompressPoint a                     = a

compressPoint :: ECPoint -> ECPoint
compressPoint (UncompressedPoint x y) = CompressedPoint x 4
compressPoint a                       = a
