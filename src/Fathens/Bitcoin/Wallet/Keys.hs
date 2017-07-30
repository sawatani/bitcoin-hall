module Fathens.Bitcoin.Wallet.Keys where

import           Control.Monad
import qualified Crypto.ECC                     as ECC
import qualified Crypto.PubKey.ECC.ECDSA        as ECDSA
import qualified Crypto.PubKey.ECC.P256         as P256
import           Crypto.PubKey.ECC.Types        (CurveName (SEC_p256k1),
                                                 CurvePrime, Point (..),
                                                 common_curve, ecc_g,
                                                 getCurveByName)
import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy           as BS
import qualified Data.ByteString.Lazy.Char8     as C8
import           Data.List
import           Data.Maybe
import           Data.Text.Lazy                 (Text)
import qualified Data.Text.Lazy                 as T
import           Data.Typeable
import           Data.Word                      (Word8)
import           Fathens.Bitcoin.Binary.Base58
import           Fathens.Bitcoin.Binary.Hash
import           Fathens.Bitcoin.Binary.Num
import qualified Fathens.Bitcoin.Wallet.Address as AD

-- Constants


-- Data

data PrivateKey = PrivateKey {
  prvPrefix :: AD.AddressPrefix
, prvK      :: Integer
} deriving (Show, Eq)

data PublicKey = PublicKey {
  pubPrefix         :: AD.AddressPrefix
, pubPoint          :: ECPoint
, pubShouldCompress :: Bool
} deriving (Show, Eq)

data ECPoint = ECPoint {
  ecPointX :: Integer
, ecPointY :: Integer
} deriving (Show, Eq)

-- Classes

-- Functions

readPrvKey :: Base58 -> Maybe PrivateKey
readPrvKey b58 = do
  prefix <- findPrefixPrv b58
  d <- decodeBase58Check b58
  payload <- AD.getPayload prefix d
  let k = fromBigEndian payload
  return $ PrivateKey prefix k

prvKeyWIF :: PrivateKey -> Base58
prvKeyWIF (PrivateKey prefix k) = enc k
  where
    enc = encodeBase58Check . AD.appendPayload prefix . b256

getPublicKey :: PrivateKey -> PublicKey
getPublicKey (PrivateKey prvPrefix k) = PublicKey {
  pubPrefix = AD.prefixP2PKH $ AD.isTestnet prvPrefix
, pubPoint = k2ec k
, pubShouldCompress = AD.isCompressed prvPrefix
}

pubKeyAddress :: PublicKey -> Base58
pubKeyAddress (PublicKey prefix ec comp) = enc ec
  where
    enc = encodeBase58Check . AD.appendPayload prefix .
          hash160Data . hash160 . encodePoint comp

-- Utilities

findPrefixPrv :: Base58 -> Maybe AD.AddressPrefix
findPrefixPrv = AD.findBySymbol [AD.prefixPRV, AD.prefixCPRV] . base58Text

encodePoint :: Bool -> ECPoint -> ByteString
encodePoint isCompress (ECPoint x y) = encoded
  where
    encoded | isCompress = 4 `BS.cons` b256 x `BS.append` b256 y
            | otherwise = z `BS.cons` b256 x
    z | odd y = 3
      | otherwise = 2

b256 :: Integer -> ByteString
b256 = toBigEndianFixed 32

k2ec :: Integer -> Maybe ECPoint
k2ec k = do
    s <- P256.scalarFromInteger k
    let p = ECC.pointSmul c s g
    let (x, y) = P256.pointToIntegers p
    return $ ECPoint x y
  where
    c = Proxy :: Proxy ECC.Curve_P256R1
    g = P256.pointFromIntegers (x, y)
    Point x y = ecc_g $ common_curve $ getCurveByName SEC_p256k1
