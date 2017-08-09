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
import           Data.Word                      (Word32, Word8)
import           Fathens.Bitcoin.Binary.Base58
import           Fathens.Bitcoin.Binary.Hash
import           Fathens.Bitcoin.Binary.Num
import qualified Fathens.Bitcoin.Wallet.Address as AD

-- Data

data PrivateKey = PrivateKey {
  prvPrefix     :: AD.Prefix
, prvK          :: Integer
, isCompressing :: Bool
}
data PublicKey = PublicKey {
  pubPrefix  :: AD.Prefix
, pubPoint   :: EC.Point
, isCompress :: Bool
}
data XPrvKey = XPrvKey PrivateKey ExtendData
data XPubKey = XPubKey PublicKey ExtendData

data ExtendData = ExtendData {
  depth             :: Word8
, parentFingerPrint :: Word32
, nodeIndex         :: Node
, chainCode         :: Bits256
} deriving (Show, Eq)

data Node = Node {
  isHardened :: Bool
, index      :: Word32
} deriving (Show, Eq)

type Bits256 = ByteString

-- Classes

-- Functions

readPrvKey :: Base58 -> Maybe PrivateKey
readPrvKey b58 = do
  prefix <- AD.findBySymbol b58
  c <- AD.isCompressing prefix
  d <- decodeBase58Check b58
  payload <- AD.getPayload prefix d
  let k = fromBigEndian payload
  return $ PrivateKey prefix k c

prvKeyWIF :: PrivateKey -> Base58
prvKeyWIF (PrivateKey prefix k c) = enc k
  where
    enc = encodeBase58Check . AD.appendPayload prefix . b256

getPublicKey :: PrivateKey -> PublicKey
getPublicKey (PrivateKey prvPrefix k c) = PublicKey p i c
  where
    p = AD.PrefixP2PKH $ AD.isTestnet prvPrefix
    i = k2ec k

pubKeyAddress :: PublicKey -> Base58
pubKeyAddress (PublicKey prefix ec c) = enc ec
  where
    enc = encodeBase58Check . AD.appendPayload prefix .
          hash160Data . hash160 . encodePoint c

-- Utilities

encodePoint :: Bool -> EC.Point -> ByteString
encodePoint isCompress (EC.Point x y) = encoded
  where
    encoded | isCompress = z `BS.cons` b256 x
            | otherwise = 4 `BS.cons` b256 x `BS.append` b256 y
    z | odd y = 3
      | otherwise = 2

b256 :: Integer -> Bits256
b256 = toBigEndianFixed 32

k2ec :: Integer -> EC.Point
k2ec k = multiply k sec_p256k1_g

multiply :: Integer -> EC.Point -> EC.Point
multiply s p = pointMul curve s p

-- Constants

curve = EC.getCurveByName EC.SEC_p256k1
sec_p256k1_g = EC.ecc_g $ EC.common_curve $ curve
