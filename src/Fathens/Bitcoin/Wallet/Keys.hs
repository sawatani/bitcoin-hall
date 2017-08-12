module Fathens.Bitcoin.Wallet.Keys (
  PrivateKey
, PublicKey
, ECKey
, ECPoint
, maxECC_K
, readPrivateKey
, prvKeyWIF
, getPublicKey
, pubKeyAddress
, ecKey
, getPublicECPoint
, encodeECPoint
, decodeECPoint
) where

import           Control.Monad
import qualified Crypto.ECC                     as ECC
import           Crypto.Error
import qualified Crypto.PubKey.ECC.P256         as P256
import           Crypto.PubKey.ECC.Prim         (pointMul)
import qualified Crypto.PubKey.ECC.Types        as EC
import           Data.Bits
import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy           as BS
import           Data.List
import           Data.Maybe
import           Data.Word                      (Word32, Word8)
import           Fathens.Bitcoin.Binary.Base58
import           Fathens.Bitcoin.Binary.Hash
import           Fathens.Bitcoin.Binary.Num
import qualified Fathens.Bitcoin.Wallet.Address as AD
import           GHC.Int                        (Int64)

-- Data

maxECC_K :: Word256
maxECC_K = fromInteger $ EC.ecc_n $ EC.common_curve $ curve

data PrivateKey = PrivateKey {
  prvPrefix     :: AD.Prefix
, prvK          :: ECKey
, isCompressing :: Bool
} deriving (Show, Eq)
data ECKey = ECKey {
  ecK :: Word256
} deriving (Show, Eq)

data PublicKey = PublicKey {
  pubPrefix  :: AD.Prefix
, pubPoint   :: ECPoint
, isCompress :: Bool
} deriving (Show, Eq)
data ECPoint = ECPoint {
  ecPointX :: Word256
, ecPointY :: Word256
} deriving (Show, Eq)

data XPrvKey = XPrvKey PrivateKey ExtendData
data XPubKey = XPubKey PublicKey ExtendData

data ExtendData = ExtendData {
  depth             :: Word8
, parentFingerPrint :: Word32
, nodeIndex         :: Node
, chainCode         :: Word256
} deriving (Show, Eq)

data Node = Node {
  isHardened :: Bool
, index      :: Word32
} deriving (Show, Eq)

-- Classes

-- Functions

-- fromHDSeed :: Word256 -> Maybe HDPrivate

readPrivateKey :: Base58 -> Maybe PrivateKey
readPrivateKey b58 = do
  prefix <- AD.findBySymbol b58
  c <- AD.isCompressing prefix
  d <- decodeBase58Check b58
  payload <- AD.getPayload prefix d
  k <- fromBigEndianFixed payload
  return $ PrivateKey prefix (ECKey k) c

prvKeyWIF :: PrivateKey -> Base58
prvKeyWIF (PrivateKey prefix (ECKey k) c) = enc k
  where
    enc = encodeBase58Check . AD.appendPayload prefix . toBigEndianFixed

getPublicKey :: PrivateKey -> PublicKey
getPublicKey (PrivateKey prvPrefix k c) = PublicKey p i c
  where
    p = AD.PrefixP2PKH $ AD.isTestnet prvPrefix
    i = getPublicECPoint k

pubKeyAddress :: PublicKey -> Base58
pubKeyAddress (PublicKey prefix ec c) = enc ec
  where
    enc = encodeBase58Check . AD.appendPayload prefix .
          hash160Data . hash160 . encodeECPoint c

ecKey :: Word256 -> Maybe ECKey
ecKey k = do
  guard $ 0 < k && k <= maxECC_K
  return $ ECKey k

getPublicECPoint :: ECKey -> ECPoint
getPublicECPoint (ECKey k) = convertPoint $ multiply (toInteger k) eccG

decodeECPoint :: ByteString -> Maybe (Bool, ECPoint)
decodeECPoint bs = do
  guard $ not (BS.null bs)
  (x, y) <- read
  return (isCompress, ECPoint x y)
  where
    h = BS.head bs
    body = BS.tail bs
    isCompress = h /= 4
    read | isCompress = readCompressed
         | otherwise = readUncompressed

    readUncompressed = do
      guard $ BS.length body >= (lenBits256 * 2)
      let (xb, yb) = BS.splitAt lenBits256 body
      x <- fromBigEndianFixed xb
      y <- fromBigEndianFixed yb
      return (x, y)

    readCompressed = do
      guard (h == 2 || h == 3)
      guard $ BS.length body >= lenBits256
      let isOdd = h == 3
      x <- fromBigEndianFixed body
      return (x, yFromX isOdd x)

encodeECPoint :: Bool -> ECPoint -> ByteString
encodeECPoint isCompress (ECPoint x y) = encoded
  where
    encoded | isCompress = z `BS.cons` toBigEndianFixed x
            | otherwise = 4 `BS.cons`
                          toBigEndianFixed x `BS.append` toBigEndianFixed y
    z | odd y = 3
      | otherwise = 2

-- Utilities

convertPoint :: EC.Point -> ECPoint
convertPoint (EC.Point x y) = ECPoint (justWord256 x) (justWord256 y)
  where
    justWord256 = fromJust . toWord256

multiply :: Integer -> EC.Point -> EC.Point
multiply s p = pointMul curve s p

yFromX :: Bool -> Word256 -> Word256
yFromX isOdd x
  | isOdd == odd y = y
  | otherwise = eccP - y
  where
    v = (7 + pow_mod x 3) `mod` eccP
    y = pow_mod v $ (eccP + 1) `div` 4
    pow_mod a b
      | b == 0 = 1
      | odd b = pm $ pow_mod a $ b - 1
      | otherwise = pow_mod (pm a) $ b `div` 2
      where
        pm z = a * z `mod` eccP

-- Constants

maxBits256 = toInteger (maxBound :: Word256)
lenBits256 = (256 `div` 8) :: Int64

curve@(EC.CurveFP cp) = EC.getCurveByName EC.SEC_p256k1
eccP = (fromInteger $ EC.ecc_p cp) :: Word256
eccG = EC.ecc_g $ EC.common_curve $ curve :: EC.Point
