module Fathens.Bitcoin.Wallet.Keys where

import           Control.Monad
import qualified Crypto.ECC                     as ECC
import           Crypto.Error
import qualified Crypto.PubKey.ECC.P256         as P256
import           Crypto.PubKey.ECC.Types        (CurveName (SEC_p256k1),
                                                 Point (..), common_curve,
                                                 ecc_g, getCurveByName)
import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy           as BS
import           Data.List
import           Data.Maybe
import           Data.Word                      (Word32)
import           Fathens.Bitcoin.Binary.Base58
import           Fathens.Bitcoin.Binary.Hash
import           Fathens.Bitcoin.Binary.Num
import qualified Fathens.Bitcoin.Wallet.Address as AD

-- Data

data PrivateKey = PrivateKey {
  prvPrefix :: AD.AddressPrefix
, prvK      :: Word32
} deriving (Show, Eq)

data PublicKey = PublicKey {
  pubPrefix         :: AD.AddressPrefix
, pubPoint          :: ECPoint
, pubShouldCompress :: Bool
} deriving (Show, Eq)

data ECPoint = ECPoint {
  ecPointX :: Word32
, ecPointY :: Word32
} deriving (Show, Eq)

-- Classes

-- Functions

readPrvKey :: Base58 -> Maybe PrivateKey
readPrvKey b58 = do
  prefix <- findPrefixPrv b58
  d <- decodeBase58Check b58
  payload <- AD.getPayload prefix d
  let k = fromInteger $ fromBigEndian payload
  return $ PrivateKey prefix k

prvKeyWIF :: PrivateKey -> Base58
prvKeyWIF (PrivateKey prefix k) = enc k
  where
    enc = encodeBase58Check . AD.appendPayload prefix . b256

getPublicKey :: PrivateKey -> PublicKey
getPublicKey prv@(PrivateKey prvPrefix k) = PublicKey {
  pubPrefix = AD.prefixP2PKH $ AD.isTestnet prvPrefix
, pubPoint = k2ec prv
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

b256 :: Word32 -> ByteString
b256 = toBigEndianFixed 32 . toInteger

k2ec :: PrivateKey -> ECPoint
k2ec prv = multiply (getK prv) sec_p256k1_g

getK :: PrivateKey -> P256.Scalar
getK (PrivateKey _ k) = scalar k
  where
    -- Word32 never fail to conversion
    scalar = fromJust . maybeCryptoError . P256.scalarFromInteger . toInteger

multiply :: P256.Scalar -> P256.Point -> ECPoint
multiply s p = toECPoint $ ECC.pointSmul curve s p

toECPoint :: P256.Point -> ECPoint
toECPoint = mkPoint . P256.pointToIntegers
  where
    mkPoint (x, y) = ECPoint (fromInteger x) (fromInteger y)

-- Constants

curve = Just ECC.Curve_P256R1

sec_p256k1_g = P256.pointFromIntegers (x, y)
  where
    Point x y = ecc_g $ common_curve $ getCurveByName SEC_p256k1
