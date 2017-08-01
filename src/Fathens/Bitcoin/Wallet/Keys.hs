module Fathens.Bitcoin.Wallet.Keys (
  PrivateKey
, PublicKey
, readPrvKey
, prvKeyWIF
, getPublicKey
, pubKeyAddress
) where

import           Control.Monad
import           Crypto.PubKey.ECC.Prim         (pointMul)
import qualified Crypto.PubKey.ECC.Types        as EC
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
, prvK      :: Integer
} deriving (Show, Eq)

data PublicKey = PublicKey {
  pubPrefix         :: AD.AddressPrefix
, pubPoint          :: EC.Point
, pubShouldCompress :: Bool
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

encodePoint :: Bool -> EC.Point -> ByteString
encodePoint isCompress (EC.Point x y) = encoded
  where
    encoded | isCompress = z `BS.cons` b256 x
            | otherwise = 4 `BS.cons` b256 x `BS.append` b256 y
    z | odd y = 3
      | otherwise = 2

b256 :: Integer -> ByteString
b256 = toBigEndianFixed 32

k2ec :: Integer -> EC.Point
k2ec k = multiply k sec_p256k1_g

multiply :: Integer -> EC.Point -> EC.Point
multiply s p = pointMul curve s p

-- Constants

curve = EC.getCurveByName EC.SEC_p256k1
sec_p256k1_g = EC.ecc_g $ EC.common_curve $ curve
