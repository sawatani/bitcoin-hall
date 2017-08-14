{-# LANGUAGE TypeFamilies #-}
module Fathens.Bitcoin.Wallet.Keys (
  PrvKey
, PubKey
, ECKey
, ECPoint
, PublicKey(..)
, PrivateKey(..)
, ExtendPublicKey(..)
, ExtendPrivateKey(..)
, maxECC_K
, ecKey
, encodeECPoint
, decodeECPoint
) where

import           Control.Monad
import qualified Crypto.ECC                     as ECC
import           Crypto.Error
import qualified Crypto.PubKey.ECC.P256         as P256
import           Crypto.PubKey.ECC.Prim         (pointAdd, pointMul)
import qualified Crypto.PubKey.ECC.Types        as EC
import           Data.Bits
import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy           as BS
import qualified Data.ByteString.Lazy.Char8     as C8
import           Data.List
import           Data.Maybe
import           Data.Word                      (Word32, Word8)
import           Fathens.Bitcoin.Binary.Base58
import           Fathens.Bitcoin.Binary.Hash
import           Fathens.Bitcoin.Binary.Num
import           Fathens.Bitcoin.Wallet.Address
import           GHC.Int                        (Int64)

-- Data

maxECC_K :: Word256
maxECC_K = fromInteger $ EC.ecc_n $ EC.common_curve $ curve

  -- PrvKey prefix key isCompressForPublicKey
data PrvKey = PrvKey Prefix ECKey Bool deriving (Show, Eq)

  -- ECKey ecK
data ECKey = ECKey Word256 deriving (Show, Eq)

  -- PubKey prefix ecPoint isCompress
data PubKey = PubKey Prefix ECPoint Bool deriving (Show, Eq)

  -- ECPoint x y
data ECPoint = ECPoint Word256 Word256 deriving (Show, Eq)

data XPrvKey = XPrvKey Prefix HDPrvKey
data HDPrvKey = HDPrvKey ExtendData ECKey

data HDPubKey = HDPubKey ExtendData ECPoint
data XPubKey = XPubKey Prefix HDPubKey

data ExtendData = ExtendData {
  depth             :: Word8
, parentFingerPrint :: Word32
, nodeIndex         :: Word32
, chainCode         :: Word256
} deriving (Show, Eq)

data HDNode
  = HDNodeNormal_ HDNodeNormal
  | HDNodeHardened_ HDNodeHardened
  deriving (Show, Eq)
data HDNodeNormal = HDNodeNormal Word32 deriving (Show, Eq)
data HDNodeHardened = HDNodeHardened Word32 deriving (Show, Eq)

decodeExtendData :: ByteString -> Maybe (ExtendData, ByteString)
decodeExtendData payload = do
  let d: f: i: c: o: [] = chopBS [1, 4, 4, 32] payload
  (depth, _) <- BS.uncons d
  fingerprint <- fromBigEndianFixed f
  index <- fromBigEndianFixed i
  chain <- fromBigEndianFixed c
  return $ (ExtendData depth fingerprint index chain, o)

encodeExtendData :: ExtendData -> ByteString
encodeExtendData (ExtendData d f i c) = d `BS.cons`
  toBigEndianFixed f `BS.append`
  toBigEndianFixed i `BS.append`
  toBigEndianFixed c

-- Instances

instance ReadFromBase58 PrvKey where
  fromBase58 b58 = do
    prefix <- findBySymbol [PrefixPRV, PrefixCPRV] b58
    c <- isCompressing prefix
    d <- decodeBase58Check b58
    payload <- getPayload prefix d
    k <- fromBigEndianFixed payload
    return $ PrvKey prefix (ECKey k) c

instance WriteToBase58 PrvKey where
  toBase58 (PrvKey prefix (ECKey k) c) = enc k
    where
      enc = encodeBase58Check . appendPayload prefix . toBigEndianFixed

instance WriteToBase58 PubKey where
  toBase58 (PubKey prefix ec c) = enc ec
    where
      enc = encodeBase58Check . appendPayload prefix .
            hash160Data . hash160 . encodeECPoint c

instance ReadFromBase58 XPrvKey where
  fromBase58 b58 = do
    prefix <- findBySymbol [PrefixXPRV] b58
    d <- decodeBase58Check b58
    payload <- getPayload prefix d
    (ed, key) <- decodeExtendData payload
    (s, v) <- BS.uncons key
    guard $ s == 0
    k <- ecKey =<< fromBigEndianFixed v
    return $ XPrvKey prefix $ HDPrvKey ed k

instance WriteToBase58 XPrvKey where
  toBase58 (XPrvKey prefix hd) = encodeBase58Check $ appendPayload prefix d
    where
      HDPrvKey ed (ECKey k) = hd
      d = encodeExtendData ed `BS.append` (0 `BS.cons` toBigEndianFixed k)

instance ReadFromBase58 XPubKey where
  fromBase58 b58 = do
    prefix <- findBySymbol [PrefixXPUB] b58
    d <- decodeBase58Check b58
    payload <- getPayload prefix d
    (ed, key) <- decodeExtendData payload
    (_, p) <- decodeECPoint key
    return $ XPubKey prefix $ HDPubKey ed p

instance WriteToBase58 XPubKey where
  toBase58 (XPubKey prefix hd) = encodeBase58Check $ appendPayload prefix d
    where
      HDPubKey ed ecPoint = hd
      d = encodeExtendData ed `BS.append` encodeECPoint True ecPoint

instance PrivateKey PrvKey where
  type PublicKeyType PrvKey = PubKey
  toPublicKey (PrvKey prvPrefix k c) = PubKey p i c
    where
      p = PrefixP2PKH $ isTestnet prvPrefix
      i = toPublicKey k

instance PrivateKey ECKey where
  type PublicKeyType ECKey = ECPoint
  toPublicKey (ECKey k) = toPoint $ k @* eccG

instance PublicKey PubKey where

instance PublicKey ECPoint where

instance ExtendPrivateKey HDPrvKey where
  type InnerPrivateKeyType HDPrvKey = ECKey
  type ExtendPublicKeyType HDPrvKey = HDPubKey

  getPrivateKey (HDPrvKey _ ecKey) = ecKey

  toExtendPublicKey (HDPrvKey ed ecKey) = HDPubKey ed $ toPublicKey ecKey

  derivePrivateKey (HDPrvKey (ExtendData depth _ _ chain) ecKey@(ECKey k)) node
    = HDPrvKey childData childKey
    where
      childData = ExtendData (depth + 1) fingerprint index childChain
      childKey = ECKey $ fromInteger $
        (toInteger k) + (toInteger l) `mod` toInteger maxECC_K

      fingerprint = mkFingerprint $ toPublicKey ecKey

      (l, childChain) = hdHash chain index body
      (index, body) = hashBody node

      hashBody (HDNodeNormal_ (HDNodeNormal index)) =
        (index, encodeECPoint True $ toPublicKey ecKey)
      hashBody (HDNodeHardened_ (HDNodeHardened index)) =
        (index, 0 `BS.cons` toBigEndianFixed k)

instance ExtendPrivateKey XPrvKey where
  type InnerPrivateKeyType XPrvKey = PrvKey
  type ExtendPublicKeyType XPrvKey = XPubKey

  getPrivateKey (XPrvKey myPrefix (HDPrvKey _ ecKey)) = PrvKey prefix ecKey True
    where
      prefix = PrefixCPRV $ isTestnet myPrefix

  toExtendPublicKey (XPrvKey myPrefix hd) = XPubKey prefix child
    where
      prefix = PrefixXPUB $ isTestnet myPrefix
      child = toExtendPublicKey hd

  derivePrivateKey (XPrvKey prefix hd) node = XPrvKey prefix child
    where
      child = derivePrivateKey hd node

instance ExtendPublicKey HDPubKey where
  type InnerPublicKeyType HDPubKey = ECPoint

  getPublicKey (HDPubKey _ p) = p

  derivePublicKey (HDPubKey (ExtendData depth _ _ chain) ecPoint)
    (HDNodeNormal index) = HDPubKey childData childPoint
    where
      childData = ExtendData (depth + 1) fingerprint index childChain
      childPoint = toPoint $ l @* eccG @+ fromPoint ecPoint

      fingerprint = mkFingerprint ecPoint

      (l, childChain) = hdHash chain index $ encodeECPoint True ecPoint

instance ExtendPublicKey XPubKey where
  type InnerPublicKeyType XPubKey = PubKey

  getPublicKey (XPubKey myPrefix (HDPubKey _ ecPoint)) =
    PubKey prefix ecPoint True
    where
      prefix = PrefixP2PKH $ isTestnet myPrefix

  derivePublicKey (XPubKey prefix hd) node = XPubKey prefix child
    where
      child = derivePublicKey hd node

hdHash :: Word256 -> Word32 -> ByteString -> (Word256, Word256)
hdHash chain index body = (toNum l, toNum r)
  where
    (l, r) = splitHalf $ hash512Data $ hmac512 (toBigEndianFixed chain) $
        body `BS.append` toBigEndianFixed index
    toNum = fromJust . fromBigEndianFixed

-- Classes

class PrivateKey a where
  type PublicKeyType a :: *
  toPublicKey :: a -> (PublicKeyType a)

class PublicKey a where

class ExtendPrivateKey a where
  type InnerPrivateKeyType a :: *
  type ExtendPublicKeyType a :: *
  getPrivateKey :: a -> (InnerPrivateKeyType a)
  toExtendPublicKey :: a -> (ExtendPublicKeyType a)
  derivePrivateKey :: a -> HDNode -> a

class ExtendPublicKey a where
  type InnerPublicKeyType a :: *
  getPublicKey :: a -> (InnerPublicKeyType a)
  derivePublicKey :: a -> HDNodeNormal -> a

-- Functions

exKeyFromSeed :: Integer -> Maybe HDPrvKey
exKeyFromSeed i | i < 2^128 = mkMaster 128
                | i < 2^256 = mkMaster 256
                | i < 2^512 = mkMaster 512
                | otherwise = Nothing
  where
    mkMaster n = fmap (HDPrvKey d) $ ecKey $ toNum l
      where
        d = ExtendData 0 0 0 $ toNum r
        (l, r) = splitHalf $ hash512Data $ hmac512 masterSeed $
                 putBigEndianFixed (n `div` 8) i
        toNum = fromJust . fromBigEndianFixed

ecKey :: Word256 -> Maybe ECKey
ecKey k = do
  guard $ 0 < k && k < maxECC_K
  return $ ECKey k

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
      let (xb, yb) = splitHalf body
      x <- fromBigEndianFixed xb
      y <- fromBigEndianFixed yb
      guard $ y == yFromX (odd y) x
      return (x, y)

    readCompressed = do
      guard (h == 2 || h == 3)
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

splitHalf :: ByteString -> (ByteString, ByteString)
splitHalf bs = BS.splitAt (BS.length bs `div` 2) bs

chopBS :: [Int64] -> ByteString -> [ByteString]
chopBS [] bs = [bs]
chopBS (n : ns) bs = h : chopBS ns t
  where
    (h, t) = BS.splitAt n bs

mkFingerprint :: ECPoint -> Word32
mkFingerprint = read4 . hash160Data . hash160 . encodeECPoint True
  where
    read4 = fromInteger . fromBigEndian . BS.take 4

toPoint :: EC.Point -> ECPoint
toPoint (EC.Point x y) = ECPoint (justWord256 x) (justWord256 y)
  where
    justWord256 = fromJust . toWord256

fromPoint :: ECPoint -> EC.Point
fromPoint (ECPoint x y) = EC.Point (toInteger x) (toInteger y)

(@*) :: Word256 -> EC.Point -> EC.Point
(@*) s p = pointMul curve (toInteger s) p

(@+) :: EC.Point -> EC.Point -> EC.Point
(@+) a b = pointAdd curve a b

yFromX :: Bool -> Word256 -> Word256
yFromX isOdd x
  | isOdd == odd y = y
  | otherwise = eccP - y
  where
    v = (7 + x `pw` 3) `mod` eccP
    y = v `pw` ((eccP + 1) `div` 4)
    pw a b
      | b == 0 = 1
      | odd b = pm $ a `pw` (b - 1)
      | otherwise = pm a `pw` (b `div` 2)
      where
        pm z = a * z `mod` eccP

-- Constants

curve@(EC.CurveFP cp) = EC.getCurveByName EC.SEC_p256k1
eccP = (fromInteger $ EC.ecc_p cp) :: Word256
eccG = EC.ecc_g $ EC.common_curve $ curve :: EC.Point

masterSeed = C8.pack "Bitcoin seed" :: ByteString
